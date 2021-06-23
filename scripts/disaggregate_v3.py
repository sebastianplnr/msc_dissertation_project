print('importing libraries')
import pandas as pd #for dealing with csv import
import os # for joining paths and filenames sensibly
import matplotlib.pyplot as plt  # Matplotlib's pyplot: MATLAB-like syntax

print('loading datafiles')
# set wkdir
file_path = '/Users/sebastian/Documents/Uni/Sheffield (MSc)/2. Semester/Research Project/msc_dissertation_project'
os.chdir(file_path)

# nb new datase
filename = os.path.join('data','CrowdstormingDataJuly1st.csv') 
df = pd.read_csv(filename)

#add new vars
df['skintone'] = (df['rater1'] + df['rater2']) / 2
df['allreds'] = df['yellowReds'] + df['redCards']
df['allredsStrict'] = df['redCards']
df['refCount'] = 0


#add a column which tracks how many games each ref is involved in
refs = pd.unique(df['refNum'].values.ravel()) #list all unique ref IDs

#for each ref, count their dyads
for r in refs:
    df['refCount'][df['refNum'] == r] = len(df[df['refNum'] == r])    

# for r in refs:
#     if len(df[df['refNum'] == refs[r]]) > 1:
#        print len(df[df['refNum'] == refs[r]])


colnames = list(df.columns)


j = 0
out = [0 for _ in range(sum(df['games']))]

for _, row in df.iterrows():
        n = row['games']
        c = row['allreds']
        d = row['allredsStrict']
        
        #row['games'] = 1        
        
        for _ in range(n):
                row['allreds'] = 1 if (c-_) > 0 else 0
                row['allredsStrict'] = 1 if (d-_) > 0 else 0
                rowlist = list(row)  #convert from pandas Series to prevent overwriting previous values of out[j]
                out[j] = rowlist
                j += 1
                if j%10 == 0:    
                    print("Number " + str(j) + " of " + str(df.shape[0]))


#pd.DataFrame(out, columns = colnames).to_csv('crowdstorm_disaggregated.csv', index = False) #have to make column headers explicit
pd.DataFrame(out, columns = colnames).to_csv(file_path + '/data/crowdstorm_disaggregated.csv', index = False) 
