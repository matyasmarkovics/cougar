# Run and Cougar

This is an Erlang script that invokes a Service, specified in the 1st argument.
Any subsequent arguments are passed onto the invoked Service.
Regardless of how generic I would like to sound,
some behaviour of this `run` script is still specific to the `cougar` service.

The `cougar` service is a word occurrence counter,
where words are separated by line-feed on standard input.
The words and they occurrences are printed on standard out in no particular order.

To run the `cougar` service you need an input file, f.x.: a log file.
> cat 'some-log-file-name' | ./run cougar 10 0

10, the first numeric argument specifies the maximum size the buffer is allowed to grow
before flushing to main storage (disk).
Using 0 instead of 10 would enable the buffer to grow infinitely large.

0, the last argument disables the functionality of flushing the buffer after a specific timeout.
Using 1000 here would flush the buffer every second.

The implementation uses a `buffer` to store intermediate results in memory.
The type of this `buffer` can either be `ets` or `dict`, prior being the default.
To use `dict`s, try:
> cat 'some-log-file-name' | BUFFER_TYPE=dict ./run cougar 10 0

Recommended command:
> time sh -c 'tail -n100000 <large_file>.tsv | cut -f3 | ./run cougar 0 1000'

## Test Run

> time sh -c 'tail -n100000 ~/lastfm-dataset-1K/userid-timestamp-artid-artname-traid-traname.tsv | cut -f1 | ./run cougar 0 1000'

real	0m22.566s
user	0m20.198s
sys     0m2.327s

cougar.erl:3: Warning: undefined callback function code_change/3 (behaviour 'gen_server')
cougar.erl:3: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
++++++++++++++++++++++++++ flush +++++++++++++++++++++++
user_000996	4940
user_000995	1617
user_000997	9506
user_000998	34149
user_000999	31415
user_001000	18373
