# Test Run
$time sh -c 'tail -n100000 ~/lastfm-dataset-1K/userid-timestamp-artid-artname-traid-traname.tsv | cut -f1 | ./run.erl 0 1500'

user_000996	4940
user_000995	1617
user_000997	9506
user_000998	34149
user_000999	31415
user_001000	18373

real	0m22.383s
user	0m19.989s
sys	0m2.391s

