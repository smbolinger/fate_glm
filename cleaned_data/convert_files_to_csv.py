
from datetime import datetime
import pandas as pd 
from pathlib import Path
# import shutil
import sys

# if len(sys.argv) > 0:
args     = sys.argv
now      = datetime.now()
now      = now.strftime("%Y%m%d_%H%M")
# direct   = args[0]

if len(args)>0:
    filename = args[1] # 1 because script is arg 0
else:
    print(">>> please pass me the name of an Excel file!")
print(">>> [INPUT] Excel file:",filename)
# fil = Path(direct, filename)
fil = Path(filename)
f   = fil.stem

# debug = False
if len(sys.argv) >2:
    # newpath = args[2]
    # newpath = f"{args[2]}_{datetime.date()}.csv" # datetime.date() needs an argument
    newpath = f"{args[2]}_{now}.csv" # datetime.date() needs an argument
    # debug = args[2]
else:
    newpath = f"{f}_{now}.csv"
# if debug: 
print(">>> [OUTPUT] csv file:", newpath)

# else:
# df = pd.DataFrame(pd.read_excel(filename))
# readf = pd.read_excel(filename)
readf = pd.read_excel(fil)
readf.to_csv(newpath)

# fnFile = open("import_name.txt", "w")
with open("import_name.txt", "w") as fnFile:
    # fnFile.write(newpath)
    # fnFile.write("nest_data/" + newpath) 
    fnFile.write("cleaned_data/" + newpath) 
# fnFile.close()