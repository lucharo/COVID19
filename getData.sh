kaggle datasets download -d sudalairajkumar/novel-corona-virus-2019-dataset --force

python3 << END

import pandas as pd
import numpy as np
from zipfile import ZipFile
import os

with ZipFile('novel-corona-virus-2019-dataset.zip', 'r') as zipObj:
   # Extract all the contents of zip file in current directory
   zipObj.extractall('RawData')
   
for file in os.listdir("RawData/"):
    globals()[file.split(".")[0]] = pd.read_csv('RawData/'+file)
    print(file)
    
END

rm -f novel-corona-virus-2019-dataset.zip
