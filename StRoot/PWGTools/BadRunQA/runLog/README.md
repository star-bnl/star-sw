## Installation

ONLY WORKS on machine with chrome installed. Does not work on RCAS unless you installed chrome manually there. I heard it's possible, but I haven't tried it.

Run `python3 -m pip install nltk prettytable selenium prompt_toolkit beautifulsoup4 pyfiglet pynput`

## Download shift log

Run `python3 shiftLog.py -i badrun.list -o shiftLog.json -br selectedbadrun.list`

shiftLog.json is for computer to read and shiftLog.txt is for human to read.

When download is completed, an interactive interface appears where you can select if a run is good or bad. The final selected list will be saved to newBadrun.list. The corresponding shift logs will be saved to newBadrun.txt

## Want human readable shift log

Run `python3 shiftLog.py -i badrun.list -o shiftLog.json -br selectedbadrun.list -ao AllLog.txt -po PosLog.txt -no NegLog.txt`

AllLog.txt contains all shift entries, PosLog.txt contains shift logs for good runs only, and NegLog.txt contains bad runs only.

## Modify selectedbadrun.list

There is no need to download shift logs again. You just need to give the json file as input

Run `python3 shiftLog.py -i shiftLog.json -o shiftLog.json -br selectedbadrun.list`

### Use AI

Run `python3 shiftLog.py -i badrun.list -o shiftLog.json -br selectedbadrun.list --useAI`

AI will consider any runs with log entry that conveys negative emotion a bad run. You just have to vet the remainning runs that doesn't convey negative tone. Empirically it saves you 40% of the work.
