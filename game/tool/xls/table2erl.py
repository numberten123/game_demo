# -*- coding: utf-8 -*-
import xlrd
import sys

reload(sys)   
sys.setdefaultencoding('utf8')  

EXPORT_TYPE_ROW  = 2
EXPORT_VALUE_NAME_ROW = 3
EXPORT_VALUE_TYPE_ROW = 4


def excle_to_lua(excle_file,export_type,output_dir='./'):
	data = xlrd.open_workbook(excle_file)
	for sheet in data.sheets():
		lua_table = []
		table_name = sheet.cell(0,0).value
		for r in range(5,sheet.nrows):
			line_value =[]
			for c in range(0,sheet.ncols):
				if sheet.cell(EXPORT_TYPE_ROW,c).value.find(export_type) > -1 :
					value_str = '["'+sheet.cell(EXPORT_VALUE_NAME_ROW,c).value + '"]' + '='
					_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
					if _type  == 'string':
						nowstr = '"' + str(sheet.cell(r,c).value) + '"'
						value_str += nowstr.replace("\n", "\\n")
					elif _type == 'number':
						v = sheet.cell(r,c).value
						if v==None or v == '':
							v=0
						if (v == round(v)):
							v = int(v)
						value_str += str(v)
					line_value.append(value_str)
			v = ',\t'.join(line_value)
			line_str = '{'+v+'}'
			lua_table.append(line_str)

		lua =  'local ' + table_name +' = {\r\n\t' + ',\r\n\t'.join(lua_table) +'\r\n} \r\nreturn '+ table_name

		f = open(output_dir + sys.argv[2] + '.lua','wb')
		f.write(lua)
		f.close()
	
	return 

def excle_to_erl(excle_file,export_type,output_dir='./output/'):
	data = xlrd.open_workbook(excle_file)
	for sheet in data.sheets():
		table_name = sheet.cell(0,0).value
		all=[]
		erl_table=[]
		temp1_str=''
		count=0
		for r in range(5,sheet.nrows):
			line_value =[]
			count=0
			for c in range(0,sheet.ncols):
				if sheet.cell(EXPORT_TYPE_ROW,c).value.find(export_type) > -1 :
					count=count+1
					if c == 0:
						v = sheet.cell(r,c).value
						if v==None or v == '':
							v=0
						if is_number(v) ==True:
							v = int(v)
						all.append(str(v))
						temp1_str=str(v)
						line_str = 'get_single('+temp1_str+') ->\n\t#' + table_name + '{\n\t\t'
					value_str = ''+sheet.cell(EXPORT_VALUE_NAME_ROW,c).value + ' = '
					_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,c).value
					if _type  == 'string':
						str1 = str(sheet.cell(r,c).value)
						if is_number(str1) ==True:
							str1 = int(float(str1))
						nowstr = '"' + str(str1) + '"'
						value_str += nowstr.replace("\n", "\\n")
					elif _type == 'number':
						v = sheet.cell(r,c).value
						if v==None or v == '':
							v=0
						if (v == round(v)):
							v = int(v)
						value_str += str(v)
					else:
						v = sheet.cell(r,c).value
						if is_number(v) ==True:
							v = int(v)
						nowstr = '' + str(v) + ''
						value_str += nowstr
					line_value.append(value_str)
			v = ',\n\t\t'.join(line_value)
			line_str = line_str+v+'\n\t};\n'
			erl_table.append(line_str)
		
		str1_str=','.join(all)
		erl =  '%%% !!! DO NOT EDIT !!!\n%%% auto generated from Excel files\n-module(' + table_name + ').\n-include("config.hrl").\n-export([get_all/0]).\n-export([get_single/1, get_rows/0, get_columns/0]).\n\nget_rows() -> ' + str(sheet.nrows-5) + '.\nget_columns() -> ' + str(count) + '.\nget_all() ->\n\t[' + str1_str + '].\n\n' + ''.join(erl_table) + 'get_single(Arg) -> throw({badarg, Arg, ' + table_name + '}).\n'
		f = open(output_dir + table_name + '.erl','wb')
		f.write(erl)
		f.close()
	
	return 



def excle_to_hrl_define(excle_file,output_dir='./'):
	data = xlrd.open_workbook(excle_file)
	for sheet in data.sheets():
		hrl_table = []
		table_name = sheet.cell(0,0).value
		for r in range(5,sheet.nrows):
			_type = sheet.cell(EXPORT_VALUE_TYPE_ROW,0).value
			value_str =''
			if _type  == 'string':
				value_str += '"' + str(sheet.cell(r,0).value) + '"'
			elif _type == 'number':
				v = sheet.cell(r,0).value
				if (v == round(v)):
					v = int(v)
				value_str += str(v)
			define_name = sheet.cell(r,1).value
			comment = sheet.cell(r,2).value
			line_str = (("-define(%s, %s). \t\t%%%% %s\r\n"%(define_name,value_str,comment)))
			hrl_table.append(line_str)

		hrl =  "-ifndef(%s).\r\n-define(%s, true).\r\n\r\n" %(sheet.cell(0,1).value,sheet.cell(0,1).value) + ''.join(hrl_table) + '\r\n-endif.'

		f = open(output_dir + table_name + '.hrl','wb')
		f.write(hrl)
		f.close()

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        pass
    return False

# excle_to_hrl_define('error_code.xlsx')
try:  
    excle_to_erl(sys.argv[1],'s') 
except Exception,e:
    print "ignore :",sys.argv[1]

# excle_to_lua("./xlsx/fish.xlsx",'c')

