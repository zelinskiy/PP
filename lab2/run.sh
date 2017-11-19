#/bin/sh

for (( t = 1; t <= 5; t++ ))
do
    echo "-------------------- TASK $t ------------------------"
    echo "bad version:"
    perf stat -e branch-misses ./main $t 0
    echo "good version:"
    perf stat -e branch-misses ./main $t 0
done
