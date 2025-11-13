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
## import the age matrix
age_data = pd.read_csv('age.csv')

# make the people object
# NB: n_agents needs to be large enough for things
#     to conform to expectations
ppl = ss.People(n_agents=1e5, age_data=age_data)

# ===========================================
# ///////////////////////////////////////////
# MIXING POOLS
# ///////////////////////////////////////////
# ===========================================
mps = ss.MixingPools(
    ##
    diseases = 'sis',
    ##
    beta = 0.1,
    ## SOURCE
    src = {'0-20': ss.AgeGroup(0, 20), 
           '20+': ss.AgeGroup(20, None)},
    ## DESTINATION
    dst = {'0-20': ss.AgeGroup(0, 20), 
           '20+': ss.AgeGroup(20, None)},
    ## CONTACT MATRIX
    n_contacts = [[1.0, 1.0], # A-A, A-B
                  [1.0, 1.0]], # B-A, B-B
)


# ===========================================
# ///////////////////////////////////////////
# Analyzer
# ///////////////////////////////////////////
# ===========================================
class infections_by_age(ss.Analyzer):
    """ Count infections by age """
    def __init__(self, age_bins=(0, 20, 100)):
        super().__init__()
        self.age_bins = age_bins
        self.mins = age_bins[:-1]
        self.maxes = age_bins[1:]
        self.hist = {k:[] for k in self.mins}
        return

    def init_pre(self, sim):
        super().init_pre(sim)
        self.infections = np.zeros(len(self.sim.people.age))
        return

    def step(self):
        age = self.sim.people.age
        disease = self.sim.diseases[0]
        for min, max in zip(self.mins, self.maxes):
            mask = (age >= min) & (age < max)
            self.hist[min].append(disease.infected[mask].sum())
        return

    def plot(self):
        plt.figure()
        x = self.sim.t.tvec
        for min, max in zip(self.mins, self.maxes):
            plt.plot(x, self.hist[min], label=f'Age {min}-{max}')
        plt.legend(frameon=False)
        plt.xlabel('Model time (days)')
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
    diseases = 'sis',
    networks = mps,
    people   = ppl,
    analyzers = infections_by_age(),
    verbose = False)

sim.run()

sim.analyzers.infections_by_age.plot()