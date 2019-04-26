# -*- coding: utf-8 -*-
import xlrd
import sys
from imp import reload

reload(sys)   
sys.setdefaultencoding('utf8')  

EXPORT_TYPE_ROW  = 2
EXPORT_VALUE_NAME_ROW = 3
EXPORT_VALUE_TYPE_ROW = 4


def excle_to_json(excle_file,export_type,output_dir='./json/'):
	data = xlrd.open_workbook(excle_file)
	sheet = data.sheets()[0]
	lua_table = []
	table_name = sheet.cell(0,0).value
	key_name = sheet.cell(0,1).value
	if len(key_name) ==0 :
		key_name = ''
	for r in range(5,sheet.nrows):
		line_value =[]
		key =''
		key_type = 'string'
		for c in range(0,sheet.ncols):
			if sheet.cell(EXPORT_TYPE_ROW,c).value.find(export_type) > -1 :
				value_str = '"'+sheet.cell(EXPORT_VALUE_NAME_ROW,c).value + '"' + ':'
				if key_name == sheet.cell(EXPORT_VALUE_NAME_ROW,c).value :
					key = sheet.cell(r,c).value
					key_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
				_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
				if _type == 'number':
					v = sheet.cell(r,c).value
					if v==None or v == '':
						v=0
					if (v == round(float(v))):
						v = int(v)
					value_str += str(v)
				elif _type == 'luatable' :
					v = sheet.cell(r,c).value
					if v==None or v == '':
						v='nil'
					else :
						v="\n" + str(sheet.cell(r,c).value).rstrip()
					value_str += v
				else :
					replace_str = str(sheet.cell(r,c).value).rstrip()
					replace_str = replace_str.replace("\n", "\\n")
					replace_str = replace_str.replace('\"', '\\"')
					nowstr = '"' + replace_str + '"'
					value_str += nowstr
				line_value.append(value_str)
		
		v = ','.join(line_value)
		line_str = '{'+v+'}'
		lua_table.append(line_str)
	json =  '[\n\t'+'\r\n\t,'.join(lua_table) +'\r\n] '
	filename = sys.argv[1].split('.')[0]
	f = open(output_dir + sys.argv[2] + '.json','wb')
	f.write(json)
	f.close()
	
	return 

def excle_to_lua(excle_file,export_type,output_dir='./json/'):
	data = xlrd.open_workbook(excle_file)
	sheet = data.sheets()[0]
	lua_table = []
	table_name = sheet.cell(0,0).value
	key_name = sheet.cell(0,1).value
	if len(key_name) ==0 :
		key_name = ''
	for r in range(5,sheet.nrows):
		line_value =[]
		key =''
		key_type = 'string'
		for c in range(0,sheet.ncols):
			if sheet.cell(EXPORT_TYPE_ROW,c).value.find(export_type) > -1 :
				value_str = '["'+sheet.cell(EXPORT_VALUE_NAME_ROW,c).value + '"]' + '='
				if key_name == sheet.cell(EXPORT_VALUE_NAME_ROW,c).value :
					key = sheet.cell(r,c).value
					key_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
				_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
				if _type == 'number':
					v = sheet.cell(r,c).value
					if v==None or v == '':
						v=0
					if (v == round(float(v))):
						v = int(v)
					value_str += str(v)
				elif _type == 'luatable' :
					v = sheet.cell(r,c).value
					if v==None or v == '':
						v='nil'
					else :
						v="\n" + str(sheet.cell(r,c).value).rstrip()
					value_str += v
				else :
					replace_str = str(sheet.cell(r,c).value).rstrip()
					replace_str = replace_str.replace("\n", "\\n")
					replace_str = replace_str.replace('\"', '\\"')
					nowstr = '"' + replace_str + '"'
					value_str += nowstr
				line_value.append(value_str)
		

		v = ',\t'.join(line_value)
		if key !='':
			if key_type == 'number':
				nowr=str(int(r-4))
				line_str = '{["_sort_index"]='+nowr+',\t'+v+'}'
				line_str = '['+str(int(key))+']='+line_str
			elif key_type == 'string':
				nowr=str(int(r-4))
				line_str = '{["_sort_index"]='+nowr+',\t'+v+'}'
				line_str = '["'+str(key)+'"]='+line_str
			else:
				raise NameError, 'error type'
		else :
			line_str = '{'+v+'}'
			
		lua_table.append(line_str)

	lua =  'local ' + table_name +' = {\r\n\t' + ',\r\n\t'.join(lua_table) +'\r\n} \r\nreturn '+ table_name
	f = open(output_dir + sys.argv[2] + '.lua','wb')
	f.write(lua)
	f.close()
	
	return 

# excle_to_hrl_define('error_code.xlsx')

excle_to_json(sys.argv[1],'c')
# excle_to_lua("./niuniu_rate_1.xlsx",'c')

