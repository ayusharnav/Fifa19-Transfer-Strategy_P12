import csv

# target_my = open('new_data_set.csv', 'w')
# with open('data_new.csv') as csv_file:
#     csv_reader = csv.reader(csv_file, delimiter=',')
#     line_count = 0
#     count=0
#     for row in csv_reader:
#         if row[5] == "" or row[6] == 0:
#             print row[1]
#             print line_count
#             count = count + 1
#         else:
#             target_my.writerows(row)
# target_my.close()

lines = []
with open('data_new.csv', 'r') as readFile:
    reader = csv.reader(readFile)
    lines = list(reader)

temp = []
count = 0

for i in lines:
    if i[5] == "" or i[6] == '0':
        #print i[5]
        print i[6]
        count = count + 1
    else:
        temp.append(i)

print count


with open('new_data_set.csv', 'w') as writeFile:
    writer = csv.writer(writeFile)
    writer.writerows(temp)

readFile.close()
writeFile.close()
