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

	times=[]
	events=[]
	for d1 in range(len(block)):
		for d2 in range(len(block[d1])):
			times.append(int(block[d1][d2].split('.')[0]))
			events.append(block[d1][d2].split('.')[1])

	output={'raw_block':block,
		'times':times,
		'events':events,
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
	base={'time':extraction['times'],'event':extraction['events'],
		'subject':extraction['subject'],'box':extraction['box'],
		'date':extraction['date'],'session_start':extraction['session_start'],
		'session_end':extraction['session_end'],'condition':extraction['condition'],
		'experiment_name':extraction['experiment_name'],
		'experiment_program':extraction['experiment_program']}
	data_frame=pd.DataFrame(base)
	# data_frame['session_time']=data_frame['time'].cumsum()/10.
	data_frame['time']=data_frame['time']/100.
	# data_frame.to_csv('file_name.csv')
	return data_frame


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
	subdirectory in base_directory (builds full_data_perla.csv)
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
	ultraglobal_df.to_csv('full_data_perla.csv')
	return ultraglobal_df






