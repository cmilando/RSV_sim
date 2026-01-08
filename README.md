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

## Notes for 12.15

contact matrix = Who aquires infection from whom
kids get sick at school then go home and get their parents sick
so this implies that 
you would need to update the location at each timestep

Also, we want state to change as a function of time?

And maybe we could se the same n_contacts matrix but have it vary by household

how to model the idea that kids can get sick at school and then impact adults at home
everyone has 

Another calibration workflow: [link](https://docs.starsim.org/user_guide/workflows_sir_calibration.html)

The calibration workflow needs data to calibrate too, thats whats missing from yours right now.

## Files

* 01_demo : basic model with a set age structure
* 02_demo : added a random urban/rural state
* 03_demo : modified the state to be conditional on age
* 04_demo : take a xlsx of the contact matrices
* 05_demo : calibration

## Setup and Run

Note: `starsim` only works with python version 3.13

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

to upgrade the .venv
```
source .venv/bin/activate
python -m pip install --upgrade ipykernel
python -m ipykernel install --user \
  --name RSV_starsim \
  --display-name "RSV_starsim (.venv)"
```

