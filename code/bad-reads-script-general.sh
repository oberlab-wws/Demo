file=$1

grep -B1 -A2 NNNNNNNNNN $file > ../data/scripted_bad_reads.txt

wc -l ../data/scripted_bad_reads.txt
echo "Script finished!"
