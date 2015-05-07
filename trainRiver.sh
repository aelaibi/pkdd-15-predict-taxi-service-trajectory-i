#!/bin/sh
echo "greetings from shell before all, will process $*"

curl -XPUT localhost:9200/_river/taxi-service-trajectory-train/_meta -d '
{
    "type" : "csv",
    "csv_file" : {
        "folder" : "/Users/Abdelhaq/dev/DataScience/Kaggle/pkdd-15-predict-taxi-service-trajectory-i/data",
        "filename_pattern" : "train.csv",
        "first_line_is_header":"true"
    }
}'