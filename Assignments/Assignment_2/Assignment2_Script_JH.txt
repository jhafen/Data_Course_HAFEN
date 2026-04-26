## create list of csv files under csv_files
csv_files=list.files('Data/', pattern = '.csv')
## outputs the number of files
length(csv_files)
## imports file and puts it under 'df'
df=read.csv("Data/wingspan_vs_mass.csv")
## gives the first 5 rows of data
head(df,5)
## creates a list of files starting with b
b_files=list.files(path='Data/',pattern = '^b',recursive = T,full.names = T)
## supposed to output the first line of each file starting with 'b'
for(file in b_files){
  row1=readLines(file,n=1)
  print(row1)
}
for (file in b_files) {
  first_line <- readLines(file, n = 1)
  cat("File:", file, "\nFirst line:", first_line, "\n\n")
}
## supposed to output the first line of all csv files
for (file in csv_files) {
  csvrow1=readLines(file,n=1)
  cat("File:", file, "\nFirst line:", first_line, "\n\n")
}