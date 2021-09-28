library('rPython')

address = "Wagon street 6"
python.exec('print("hello world!")')

python.exec('import pip')
python.exec("pip.main(['install','usaddress'])")

python.exec('import usaddress')
python.exec('import sys')
python.exec('print(sys.version_info)')

python.exec(paste0("addressObject = usaddress.parse('",address,"')"))

add <- python.get( "addressObject" )