# RSV_sim

[DOCS](https://docs.starsim.org/tutorials/t5_networks.html)

TODO:
* Next step is figuring out the 2d mixing matri
* then calibration against real data

## Setup
only works with version 3.13

Create a virtual environment with v3.13 with homebrew, after navigating to the Github folder
```
brew install python@3.13
/opt/homebrew/python3.13 -m venv .venv
```

Activate the environment
```
source .venv/bin/activate
```

To install the requrirements do
```
pip install -r requirements.txt
```

and, if a new library is added, freeze it
```
pip freeze > requirements.txt
```

And I've been using jupyter labs
```
jupyter lab
```

