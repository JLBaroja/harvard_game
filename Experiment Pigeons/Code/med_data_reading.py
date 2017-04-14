import pandas as pd
import os


def caterpillar(file_name,array):
	"""
	Function that reads file 'file_name' and
	returns data contained in 'array'.
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
		#print line.split()
		if found:
			if len(line.split())==1:
				break
			block.append(line.split()[1:len(line.split())])
		else:
			if line.startswith(array_name):
				# magic_number=len(line.split())
				found=True
				#block='Start'
	# print magic_number

	#times=[]
	#events=[]
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
	# return block, times, events
	# print block
	# print type(block)
	# print len(block)
	# print block[len(block)-1]
	# print block[len(block)-2]
	# print block[0]
	# #print block[[0,3,13]]
	# return subject, experiment, group, file_name#, block
 	# for b in len(block):
 		# print b


def hatter(file_name,array):
	"""
	Function that extracts certain array from certain
	MED file and returns it in a pandas dataframe
	"""
	extraction=caterpillar(file_name,array)
	base={'before_point':extraction['before_point'],'after_point':extraction['after_point'],
		'subject':extraction['subject'],'box':extraction['box'],
		'date':extraction['date'],'session_start':extraction['session_start'],
		'session_end':extraction['session_end'],'condition':extraction['condition'],
		'experiment_name':extraction['experiment_name'],
		'experiment_program':extraction['experiment_program']}
	data_frame=pd.DataFrame(base)
	data_frame=data_frame.ix[(data_frame['after_point']!='000')]
	# data_frame['session_time']=data_frame['time'].cumsum()/10.
	# data_frame['time']=data_frame['time']/100.
	# data_frame.to_csv('file_name.csv')
	return data_frame


def real_time(file_name):
	"""
	Extracts event ocurrence in real time
	"""
	events={'010':'session_start',
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
	df=hatter(file_name,'A')
	df['event']=0
	df['session_time']=0.
	for ii in range(len(df)):
		df['event'][ii]=events[df['after_point'][ii]]
		df['session_time'][ii]=float(df['before_point'][ii])/100.
	return df


def alice(files,array):
	"""
	Function that extracts data from array from all files in list
	and in current directory and merges them in a single dataframe
	"""
	global_df=pd.DataFrame()
	for arc in range(len(files)):
		frames=[global_df,hatter(files[arc],'C')]
		global_df=pd.concat(frames)
	return global_df


def cheshire(base_directory,array):
	"""
	Function that makes a .csv with data from all files in each
	subdirectory in base_directory (builds full_data.csv)
	"""
	subdirs=[x[0] for x in os.walk(base_directory)]
	ultraglobal_df=pd.DataFrame()
	for dire in subdirs[1:len(subdirs)]:
		os.chdir(dire)
		for i in os.listdir(dire):
			if i=='.DS_Store' or i=='.Rhistory':
				os.unlink(i)
		frames=[ultraglobal_df,alice(os.listdir(dire),array)]
		ultraglobal_df=pd.concat(frames)
	os.chdir(base_directory)
	ultraglobal_df.to_csv('full_data.csv')
	return ultraglobal_df






