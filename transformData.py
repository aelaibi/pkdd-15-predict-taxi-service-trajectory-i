
import json

from datetime import datetime

train = 'data/train.csv.processing.imported'

test = 'data/test.csv'

# drop some training data that doesnt have end-points
#train = train[train['LATF'] != -999]
#train = train[train['LONGF'] != -999]

#"TRIP_ID","CALL_TYPE","ORIGIN_CALL","ORIGIN_STAND","TAXI_ID","TIMESTAMP","DAY_TYPE","MISSING_DATA","POLYLINE"
header = "TRIP_ID,CALL_TYPE,ORIGIN_CALL,ORIGIN_STAND,TAXI_ID,TIMESTAMP,DAY_TYPE,MISSING_DATA,LONG1,LAT1,LONG2,LAT2"
yHead = ",LONGF,LATF"
extraCols =",MONTH,WEEK,DAY,HOUR"

def data(path, traindata=False):
    for t, line in enumerate(open(path)):
        if t == 0:
            continue
        # parse x
        line2 = ""
        d = 1
        for m, feat in enumerate(line.rstrip().replace('NA', '""').replace('","',';').replace('"','').split(';')):
            if m == 8: # POLYLINE
                polyline = json.loads(feat)
                try:
                    lat1 = polyline[0][1]
                except:
                    lat1 = -999
                try:
                    lat2 = polyline[-2][1]
                except:
                    lat2 = -999

                try:
                    long1 = polyline[0][0]
                except:
                    long1 = -999
                try:
                    long2 = polyline[-2][0]
                except:
                    long2 = -999
                line2+=",%s,%s,%s,%s" % (long1, lat1, long2, lat2)
                line2+=",%s,%s,%s,%s" % (d.month, d.isocalendar()[1], d.day, d.hour)
                if traindata:
                    try:
                        long_final = polyline[-1][0]
                    except:
                        long_final = -999
                    try:
                        lat_final = polyline[-1][1]
                    except:
                        lat_final = -999
                    #append data
                    line2+=",%s,%s" % (long_final, lat_final)

            elif m == 5:
                d = datetime.fromtimestamp(int(feat))
                line2+=feat if m == 0 else ","+feat
            else :
                line2+=feat if m == 0 else ","+feat


        yield (line2)

print 'starting ...'
tt = 1

print 'transforming train data'
with open("data/train2.csv", 'w') as outfile:
    outfile.write(header+extraCols+yHead+'\n')
    for line in data(train, traindata = True):
        outfile.write('%s\n' % line)
        if tt % 100000 == 0:
                print('%s\tencountered: %d' % (datetime.now(), tt))
        tt += 1
    outfile.close()

tt = 1
print 'transforming test data'
with open("data/test2.csv", 'w') as outfile:
    outfile.write(header+extraCols+'\n')
    for lineTest in data(test):
        outfile.write('%s\n' % lineTest)
        if tt % 100000 == 0:
                print('%s\tencountered: %d' % (datetime.now(), tt))
        tt += 1
    outfile.close()