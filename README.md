# RSV_sim

[DOCS](https://docs.starsim.org/tutorials/t5_networks.html)

TODO:

* contact matrices for every age [link](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009098)
> and the link that Allison sent

* then calibration against real data [link](https://docs.starsim.org/user_guide/workflows_calibration.html)
> you can make a dummy example for this that gets you back what you put in

> next steps:
> * make a simplified block contact matrix
> * run the full pipeline
> * create some simulated data
> * do a calibration step to get back out the inputs
> * change model type to sier?
> * then start applying with real data

## Files

* 01_demo : basic model with a set age structure
* 02_demo : added a random urban/rural state
* 03_demo : modified the state to be conditional on age
* 04_demo : take a xlsx of the contact matrices

## Setup and Run

Note: only works with python version 3.13

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

