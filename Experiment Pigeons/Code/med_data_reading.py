import pandas as pd
import os


def caterpillar(file_name,array):
	"""
	Function that reads file 'file_name' and
	returns data contained in 'array' as a dict.
	(Should work for any MED file.)
	"""
	archive=open(file_name)
	found=False
	block=[]
	array_name=array+':'
	for line in archive:
		if line.startswith('Subject:'):
			subject=line.split()[1]
		if line.startswith('Experiment:'):
			experiment=line.split()[1]
		if line.startswith('Group:'):
			group=line.split()[1]
		if line.startswith('Box:'):
			box=line.split()[1]
		if line.startswith('Start Date:'):
			date=line.split()[2]
		if line.startswith('MSN:'):
			msn=line.split()[1]
		if line.startswith('Start Time:'):
			start_time=line.split()[2]
		if line.startswith('End Time:'):
			end_time=line.split()[2]
		if found:
			if len(line.split())==1:
				break
			block.append(line.split()[1:len(line.split())])
		else:
			if line.startswith(array_name):
				found=True

	before_point=[]
	after_point=[]
	for d1 in range(len(block)):
		for d2 in range(len(block[d1])):
			before_point.append(block[d1][d2].split('.')[0])
			after_point.append(block[d1][d2].split('.')[1])

	output={'raw_block':block,
		'before_point':before_point,
		'after_point':after_point,
		'condition':group,
		'experiment_name':experiment,
		'experiment_program':msn,
		'subject':subject,
		'box':box,
		'date':date,
		'session_start':start_time,
		'session_end':end_time}

	return output


def hatter(file_name,array):
	"""
	Function that extracts certain array from certain
	MED file and returns it in a pandas dataframe
	"""
	extraction=caterpillar(file_name,array)
	base={'before_point':extraction['before_point'],
		'after_point':extraction['after_point'],
		'subject':extraction['subject'],
		'box':extraction['box'],
		'date':extraction['date'],
		'session_start':extraction['session_start'],
		'session_end':extraction['session_end'],
		'condition':extraction['condition'],
		'experiment_name':extraction['experiment_name'],
		'experiment_program':extraction['experiment_program']}
	data_frame=pd.DataFrame(base)
	data_frame=data_frame.ix[(data_frame['after_point']!='000')]
	data_frame['data_file']=file_name
	return data_frame


def trial_data(file_name):
	"""
	Extracts info at trial level
	"""
	# Trial-level info is stored in array B
	ht=hatter(file_name,'B')
	trls=ht['after_point'].unique()
	tl=pd.DataFrame(columns=['trial',
				'choice',
				'key',
				'result',
				'game_state',
				'scheduled_probability',
				'rt_central',
				'rt_choice',
				'time_start',
				'time_end'],index=range(int(max(trls))))

	tl_index=0
	for tt in trls:
		ld=ht[ht.after_point==tt]
		if len(ld)==8:
			tl.trial[tl_index]=tt
			tl.time_start[tl_index]=(float(ld.before_point[ld.index[0]])-50000000)/100
			tl.time_end[tl_index]=(float(ld.before_point[ld.index[7]])-90000000)/100
			tl.rt_central[tl_index]=(float(ld.before_point[ld.index[2]])-70000000)/100
			tl.rt_choice[tl_index]=(float(ld.before_point[ld.index[4]])-60000000)/100
			tl.scheduled_probability[tl_index]=(float(ld.before_point[ld.index[5]])-80000000)/10000
			tl.game_state[tl_index]=int(ld.before_point[ld.index[1]])-99000
			if ld.before_point[ld.index[3]]=='530':
				tl.choice[tl_index]='melioration'
				if ht['experiment_program'].unique()[0].split('_')[3]=='MAXinRIGHT':
					tl.key[tl_index]='left'
				elif ht['experiment_program'].unique()[0].split('_')[3]=='MAXinLEFT':
					tl.key[tl_index]='right'
			elif ld.before_point[ld.index[3]]=='510':
				tl.choice[tl_index]='maximization'
				if ht['experiment_program'].unique()[0].split('_')[3]=='MAXinRIGHT':
					tl.key[tl_index]='right'
				elif ht['experiment_program'].unique()[0].split('_')[3]=='MAXinLEFT':
					tl.key[tl_index]='left'
			if ld.before_point[ld.index[6]]=='690':
				tl.result[tl_index]='rewarded'
			elif ld.before_point[ld.index[6]]=='790':
				tl.result[tl_index]='not_rewarded'
			tl_index=tl_index+1
	tl['subject']=ht['subject'].unique()[0]
	tl['box']=ht['box'].unique()[0]
	tl['date']=ht['date'].unique()[0]
	tl['med_program_file']=ht['experiment_program'].unique()[0]
	tl['session']=file_name.split('_')[1]
	return tl

"""
\event key (real Time array)
\T.01: Session start
\T.02: Session end
\T.11: Response in maximization key
\T.12: Response in central key
\T.13: Response in melioration key
\T.14: Trial end
\T.15: Trial start
\T.16: Maximization light ON
\T.17: Central light ON
\T.18: Melioration light ON
\T.19: Feeder ON
\T.21: Maximization light OFF
\T.22: Central light OFF
\T.23: Melioration light OFF
\T.24: Feeder OFF
\T.35: Chamber light ON
\T.36: Chamber light OFF

\event key (trial array)
\510.ttt: Chose maximization
\530.ttt: Chose melioration
\690.ttt: Trial rewarded
\790.ttt: Trial not rewarded
\990XX.ttt: State of the game (that operated in THIS trial)
\800XXXXX.ttt: Scheduled P (given a state AND a choice)
\70XXXXXX.ttt: Reaction time central light
\60XXXXXX.ttt: Reaction time alternatives
\50XXXXXX.ttt: Time of trial start
\90XXXXXX.ttt: Time of trial end
"""


def real_time_extractor(file_name,z_pulses=False):
	"""
	Extracts event ocurrence in real time
	"""
	events={'010':'session_start',
		'020':'session_end',
		'110':'resp_max_key',
		'120':'resp_central_key',
		'130':'resp_mel_key',
		'140':'trial_end',
		'150':'trial_start',
		'160':'max_light_on',
		'170':'central_light_on',
		'180':'mel_light_on',
		'190':'feeder_on',
		'210':'max_light_off',
		'220':'central_light_off',
		'230':'mel_light_off',
		'240':'feeder_off',
		'350':'chamber_light_on',
		'360':'chamber_light_off'}
	target_array='A'

	if z_pulses:
		events={'010':'z001',
			'020':'z002',
			'030':'z003',
			'040':'z004',
			'050':'z005',
			'060':'z006'}
		target_array='Z'

	df=hatter(file_name,target_array)
	df['event']=0
	df['session_time']=df['before_point'].astype(float)/100

	for ee in events.keys():
		df['event'][df['after_point']==ee]=events[ee]

	return df


def real_time_data(file):
	rt=real_time_extractor(file)
	zp=real_time_extractor(file,z_pulses=True)
	frames=[rt,zp]
	df=pd.concat(frames)
	return df


def data_frame_merger(extracting_function,name_text_file):
	"""
	Makes a single DF according to 'function' with information 
	contained in all files in 'Raw MED files'.
	"""
	os.chdir('Raw MED files/')
	files=os.listdir('.')
	global_df=pd.DataFrame()
	for archive in range(len(files)):
		# Trial-level and real-time-level functions work with a single argument now
		frames=[global_df,extracting_function(files[archive])]
		global_df=pd.concat(frames)
	os.chdir('..')
	global_df.to_csv(name_text_file)
	#return global_df






#def alice(files,array):
#	"""
#	Function that extracts data from array from all files in list
#	and in current directory and merges them in a single dataframe
#	"""
#	global_df=pd.DataFrame()
#	for arc in range(len(files)):
#		frames=[global_df,hatter(files[arc],'C')]
#		global_df=pd.concat(frames)
#	return global_df


#def cheshire(base_directory,array):
#	"""
#	Function that makes a .csv with data from all files in each
#	subdirectory in base_directory (builds full_data.csv)
#	"""
#	subdirs=[x[0] for x in os.walk(base_directory)]
#	ultraglobal_df=pd.DataFrame()
#	for dire in subdirs[1:len(subdirs)]:
#		os.chdir(dire)
#		for i in os.listdir(dire):
#			if i=='.DS_Store' or i=='.Rhistory':
#				os.unlink(i)
#		frames=[ultraglobal_df,alice(os.listdir(dire),array)]
#		ultraglobal_df=pd.concat(frames)
#	os.chdir(base_directory)
#	ultraglobal_df.to_csv('full_data.csv')
#	return ultraglobal_df

