grep -B1 -A2 NNNNNNNNNN ../data/untrimmed_fastq/*.fastq > ../data/scripted_bad_reads.txt

wc -l ../data/scripted_bad_reads.txt
echo "Script finished!"
