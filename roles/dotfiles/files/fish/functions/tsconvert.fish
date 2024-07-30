function tsconvert --description "Convert unix timestamp to UTC time"
    set -l timestamp $argv[1]
    if test -z "$timestamp"
        echo "Usage: tsconvert <timestamp>"
        return 1
    end

    set -l utc_time (python3 -c "import datetime; timestamp = '$timestamp'.replace(',', ''); print(datetime.datetime.fromtimestamp(int(timestamp) / (1e3 if len(timestamp) > 10 else 1), datetime.UTC).strftime('%Y-%m-%d %H:%M:%S UTC'))")

    echo $utc_time
end
