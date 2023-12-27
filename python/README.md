# Ulam Spiral in Python
## Environment

The scripts were written using python version 3.12.0 (as specified on the .python-version file included in the repository). All dependencies are declared in the `requirements.txt` file, and can be installed by using pip: `pip install -r requirements.txt`.

## How to run the scrips
Once all the dependencies are satisfied, the script can be executed on console by using `python ulam-spiral.py N` where N is the number size of the desired square. For example, `python ulam-spiral.py 2` will create a $2\times2$ matrix. 

## Unit Tests
The code include unit tests for one spiral, to compare generation and some number positions. To run them, you can execute the command `python -m tests.tests` from the root directory of the project, once the dependencies are installed.