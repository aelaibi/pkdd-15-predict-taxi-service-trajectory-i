import csv
from datetime import date

# Definition of Train and Test Path

location_train = "/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/train.csv"
location_test = "/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/test.csv"

location_train_vw = "/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/taxi.train.vw"  #will be created
location_test_vw = "/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/taxi.test.vw"  #will be created


#creates Vowpal Wabbit-formatted file from tsv file
def to_vw(location_input_file, location_output_file, test=False):
    print "\nReading:", location_input_file, "\nWriting:", location_output_file
    clicks = 0
    no_clicks = 0
    with open(location_input_file) as infile, open(location_output_file, "wb") as outfile:

        reader = csv.DictReader(infile)
        for row in reader:
            #if test set label doesnt matter/or isnt available
            if test:
                label = "1"
            else:
                try:
                    if int(row['click']) == 1:
                        label = 1
                        clicks += 1
                    else:
                        label = -1
                        no_clicks += 1
                except:
                    continue

            a = row['hour']
            new_date = date(int("20" + a[0:2]), int(a[2:4]), int(a[4:6]))
            day = new_date.strftime("%A")
            dict_week = {'Sunday': 1, 'Monday': 2, 'Tuesday': 3, 'Wednesday': 4, 'Thursday': 5, 'Friday': 6,
                         'Saturday': 7}
            day = str(dict_week[day])
            hour = a[6:8]

            outfile.write(
                str(label) +
                " '" + row['id'] +
                " |C" + " C1:" + row['C1'] + " C14:" + row['C14'] + " C15:" + row['C15'] + " C16:" + row[
                    'C16'] + " C17:" + row['C17'] +
                " C18:" + row['C18'] + " C19:" + row['C19'] + " C20:" + row['C20'] + " C21:" + row['C21'] +
                " |O1" + " day:" + day + " hour:" + hour + " banner_pos:" + row['banner_pos'] +
                " |O2" + " " + row['site_id'] + " " + row['site_domain'] +
                " " + row['site_category'] + " " + row['app_id'] + " " + row['app_domain'] + " " + row['app_category'] +
                " " + row['device_id'] + " " + row['device_ip'] + " " + row['device_model'] +
                " |O2" + " device_type:" + row['device_type'] + " device_conn_type:" + row['device_conn_type'] +
                "\n")
    print 'clicks: ', clicks
    print 'no_clicks: ', no_clicks


#to_vw_cross_validation(location_train, location_train_vw, location_validation_vw)
to_vw(location_train, location_train_vw)
to_vw(location_test, location_test_vw, test=True)