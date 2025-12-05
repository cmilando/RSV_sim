import numpy as np
import pandas as pd
import sciris as sc
import starsim as ss
import matplotlib.pyplot as plt

# ===========================================
# ///////////////////////////////////////////
# AGE MATRIX -> People object
#
# there is some randomness which is introduced here
# right now i just have people in ages 10 and 20 so it
# splits across the break in age groups below
# ///////////////////////////////////////////
# ===========================================
# import the age matrix
age_data = pd.read_csv('age.csv')

# create a location state
def urban_function(n):
    """ Make a function to randomly assign people 
        to urban/rural locations """ 
    return np.random.choice(a=[True, False], p=[0.5, 0.5], size=n)
urban = ss.BoolState('urban', default=urban_function)

# make the people object
# NB: (1) n_agents needs to be large enough for things
#     to conform to expectations
#     (2) added a location state that we can use later
n_agents = 1e5
ppl = ss.People(n_agents=n_agents, 
                age_data=age_data,
                extra_states=urban)

# ===========================================
# ///////////////////////////////////////////
# MIXING POOLS
# ///////////////////////////////////////////
# ===========================================
mps = ss.MixingPools(
    
    # Options for this are: 'sir', 'sis', ...
    diseases = 'sir',
    
    # overall transmission via these mixing pools
    # NB: Need to understand what this actually means
    beta = 1.2,
    
    # SOURCE
    # NB: Make these a lambda sim so you can define locations               
    src = {'0-20 - URBAN': lambda sim: ((sim.people.age < 20) & 
                                        (sim.people.urban == True)).uids, 
           '0-20 - RURAL': lambda sim: ((sim.people.age < 20) & 
                                        (sim.people.urban == False)).uids,
           '20+ - URBAN': lambda sim: ((sim.people.age >= 20) &
                              (sim.people.urban == True)).uids,
           '20+ - RURAL': lambda sim: ((sim.people.age >= 20) &
                              (sim.people.urban == False)).uids},
    
    # DESTINATION
    dst = {'0-20 - URBAN': lambda sim: ((sim.people.age < 20) & 
                                        (sim.people.urban == True)).uids, 
           '0-20 - RURAL': lambda sim: ((sim.people.age < 20) & 
                                        (sim.people.urban == False)).uids,
           '20+ - URBAN': lambda sim: ((sim.people.age >= 20) &
                              (sim.people.urban == True)).uids,
           '20+ - RURAL': lambda sim: ((sim.people.age >= 20) &
                              (sim.people.urban == False)).uids},
    
    # CONTACT MATRIX
    # the column is destination, the row is source
    # dest: A  B
    #      [0, 0]
    #      [1, 0] 
    # means coming from B -> A, so only A increases
    n_contacts = np.multiply(
        [[1.0, 1.0, 1.0, 1.0],
         [10.0, 1.0, 1.0, 1.0],
         [1.0, 1.0, 1.0, 1.0],
         [1.0, 1.0, 1.0, 1.0]], 10)
)


# ===========================================
# ///////////////////////////////////////////
# Analyzer
# ///////////////////////////////////////////
# ===========================================
class infections_by_grp(ss.Analyzer):
    """ Count infections by age and location"""
    def __init__(self, age_bins=(0, 20, 100)):
        super().__init__()
        self.age_bins = age_bins
        self.mins = age_bins[:-1]
        self.maxes = age_bins[1:]
        self.hist = {
            min_age: {'urban': [], 'rural': []}
            for min_age in self.mins
        }
        return

    def init_pre(self, sim):
        super().init_pre(sim)
        self.infections = np.zeros(len(self.sim.people.age))
        return

    def step(self):
        age = self.sim.people.age
        urban = self.sim.people.urban  
        disease = self.sim.diseases[0]
        for min_age, max_age in zip(self.mins, self.maxes):
            mask_age = (age >= min_age) & (age < max_age)

            # Urban
            mask_urban = (mask_age) & (urban == True)
            self.hist[min_age]['urban'].append(disease.infected[mask_urban].sum())

            # Rural
            mask_rural = (mask_age) & (urban == False)
            self.hist[min_age]['rural'].append(disease.infected[mask_rural].sum())
            
        return

    def plot(self):
        plt.figure()
        x = self.sim.t.tvec
        for min_age, max_age in zip(self.mins, self.maxes):
            # Urban line
            plt.plot(
                x,
                self.hist[min_age]['urban'],
                label=f'Age {min_age}-{max_age} urban'
            )

            # Rural line (dashed)
            plt.plot(
                x,
                self.hist[min_age]['rural'],
                linestyle='--',
                label=f'Age {min_age}-{max_age} rural'
            )
        plt.legend(frameon=False)
        plt.xlabel('Model time')
        plt.ylabel('Individuals infected')
        plt.ylim(bottom=0)
        sc.boxoff()
        plt.show()
        return


# ===========================================
# ///////////////////////////////////////////
# RUN
# ///////////////////////////////////////////
# ===========================================
sim = ss.Sim(
    diseases = 'sir',
    networks = mps,
    people   = ppl,
    analyzers = infections_by_grp(),
    start = 2000,
    stop = 2010,
    dt = 0.1)

# do this step
sim.init()

# now you can set conditional state ???
for i in range(int(n_agents)):
    if sim.people.age[i] > 19:
        sim.people.urban[i] = True
    else:
        sim.people.urban[i] = False

sim.people.to_df()

#
sim.run()

sim.analyzers.infections_by_grp.plot()
