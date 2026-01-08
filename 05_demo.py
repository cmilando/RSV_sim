import numpy as np
import pandas as pd
import sciris as sc
import starsim as ss
import random
import matplotlib.pyplot as plt

def make_sim():
    # ===============================================================================
    # ///////////////////////////////////////////////////////////////////////////////
    # AGE MATRIX -> People object
    #
    # there is some randomness which is introduced here
    # right now i just have people in ages 10 and 20 so it
    # splits across the break in age groups below
    # ///////////////////////////////////////////////////////////////////////////////
    # ===============================================================================
    # import the age matrix
    age_data = pd.read_csv('age.csv')
    
    # create a location state
    def location_fcn(n):
        """ 
        Make a function to assign people to locations.
        This ultimately happens after the sim.init() step because
        this state is conditional on age. So for now just put a placeholder
        and you can randomize later with different probabilities.
        But the locations will be
        0 - household
        1 - school
        2 - community
        """ 
        locs = [-1]
        return np.random.choice(locs, size = n)
        
    location = ss.FloatArr('location', default=location_fcn)
    
    # make the people object
    # NB: (1) n_agents needs to be large enough for things
    #     to conform to expectations
    #     (2) added a location state that we can use later
    n_agents = 1e5
    ppl = ss.People(n_agents=n_agents, 
                    age_data=age_data,
                    extra_states=location)
    
    # ===============================================================================
    # ///////////////////////////////////////////////////////////////////////////////
    # MIXING POOLS
    # 
    # The first step was making them lambda functions so they can take a state
    #
    # Then writing a function that makes lambda functions 
    #
    # Finally, building the block contact matrix, largely the identity in blocks
    #
    # ///////////////////////////////////////////////////////////////////////////////
    # ===============================================================================
    
    # first define the base function
    # which defines people by age group and by location
    # TODO: add a within-family
    def in_grp(simobj, age_min, age_max, check_location):
        return ((simobj.people.age >= age_min) & (simobj.people.age < age_max) &
                (simobj.people.location == check_location)).uids
    
    # then create the two dictionaries
    age_breaks = [0, 20, 100] 
    locations = ['HOUSEHOLD', 'SCHOOL', 'COMMUNITY']
    base_dict = {}
    for loc_i, loc in enumerate(locations):
        for age_i in range(1, len(age_breaks)):
            age_min = age_breaks[age_i-1]
            age_max = age_breaks[age_i]
            nm = f'{age_min}-{age_max} - {loc}'
            # need to pass these things in as function arguments
            # if you define their defaults it works
            base_dict[nm] = lambda xsim, li=loc_i, ai=age_min, ax=age_max: \
                in_grp(xsim, ai, ax, li)
    
    # Now read in the contact matrix
    # the column is destination, the row is source
    # dest: A  B
    #      [0, 0]
    #      [1, 0] 
    # means coming from B -> A, so only A increases
    n_contacts_data = pd.read_csv('contact_matrix.csv', header=None)
    
    n_contacts_data = [[1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
             [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
             [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
             [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
             [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
             [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]]
    
    # scale it so represents the number of individuals contacting in each ?
    n_contacts = np.multiply(n_contacts_data, 10)
    
    
    mps = ss.MixingPools(
        
        # Options for this are: 'sir', 'sis', ...
        diseases = 'sir',
        
        # overall transmission via these mixing pools
        # NB: Need to understand what this actually means
        beta = 1.2,
        
        # SOURCE             
        src = base_dict,
        
        # DESTINATION
        dst = base_dict,
        
        # CONTACT MATRIX
        n_contacts = n_contacts
    )
    
    
    # ===============================================================================
    # ///////////////////////////////////////////////////////////////////////////////
    # Analyzer
    # ///////////////////////////////////////////////////////////////////////////////
    # ===============================================================================
    class infections_by_grp(ss.Analyzer):
        """ Count infections by age and location"""
        def __init__(self, age_bins=(0, 20, 100)):
            super().__init__()
            self.age_bins = age_bins
            self.mins = age_bins[:-1]
            self.maxes = age_bins[1:]
            self.hist = {
                min_age: {'hh': [], 'sch': [], 'com': []}
                for min_age in self.mins
            }
            return
    
        def init_pre(self, sim):
            super().init_pre(sim)
            self.infections = np.zeros(len(self.sim.people.age))
            return
    
        def step(self):
            age = self.sim.people.age
            location = self.sim.people.location
            disease = self.sim.diseases[0]
            for min_age, max_age in zip(self.mins, self.maxes):
                mask_age = (age >= min_age) & (age < max_age)
    
                # Household
                mask_hh = (mask_age) & (location == 0)
                self.hist[min_age]['hh'].append(disease.infected[mask_hh].sum())
    
                # School
                mask_com = (mask_age) & (location == 1)
                self.hist[min_age]['sch'].append(disease.infected[mask_com].sum())
    
                # Community
                mask_sch = (mask_age) & (location == 2)
                self.hist[min_age]['com'].append(disease.infected[mask_sch].sum())
                
            return
    
        def plot(self):
            plt.figure()
            x = self.sim.t.tvec
            for min_age, max_age in zip(self.mins, self.maxes):
                # Household line
                plt.plot(
                    x,
                    self.hist[min_age]['hh'],
                    linestyle='solid',
                    label=f'Age {min_age}-{max_age} household'
                )
                # Community line (dashed)
                plt.plot(
                    x,
                    self.hist[min_age]['com'],
                    linestyle='dashed',
                    label=f'Age {min_age}-{max_age} community'
                )
                # School line
                plt.plot(
                    x,
                    self.hist[min_age]['sch'],
                    linestyle='dotted',
                    label=f'Age {min_age}-{max_age} school'
                )
            plt.legend(frameon=False)
            plt.xlabel('Model time')
            plt.ylabel('Individuals infected')
            plt.ylim(bottom=0)
            sc.boxoff()
            plt.show()
            return
    
    
    # ===============================================================================
    # ///////////////////////////////////////////////////////////////////////////////
    # Run
    # ///////////////////////////////////////////////////////////////////////////////
    # ===============================================================================
    sim = ss.Sim(
        diseases = 'sir',
        networks = mps,
        people   = ppl,
        analyzers = infections_by_grp(),
        start = 2000,
        stop = 2010,
        dt = 0.1)
    
    # do this step first
    sim.init()
    
    # now you can see that people is initialized
    sim.people.to_df()
    
    # now you can set conditional location state based on age
    # random.choices(locations, weights,k)
    # and remember that these are the locations
    #     0 - household
    #     1 - school
    #     2 - community
    for i in range(int(n_agents)):
        if sim.people.age[i] < 19:
            sim.people.location[i] = \
                  random.choices([0, 1, 2], [1.0, 0.0, 0.0], k=1)[0] # [.33, .33, .33]
        else:
            sim.people.location[i] = \
                  random.choices([0, 1, 2], [0.0, 0.0, 1.0], k=1)[0] # [.20, .10, .70]
    
    # and now return
    return sim


# ===============================================================================
# ///////////////////////////////////////////////////////////////////////////////
# Calibration
# ///////////////////////////////////////////////////////////////////////////////
# ===============================================================================
  
# Imports and settings
debug = False # If true, will run in serial

# just trying to find beta since contact mat is given
# i suppose we could do init prev too but unsure where that goes
# notes on beta -- For example, increasing beta from 0.01 to 0.02 should double disease 
# transmission, but increasing from 0.11 to 0.12 will have a small effec
calib_pars = dict(
    beta = dict(low=0.01, high=2.0, guess=1.0, 
                suggest_type='suggest_float', log=True)
)

def build_sim(sim, calib_pars, n_reps=1, **kwargs):
    """
    Modify the base simulation by applying calib_pars. The result can be a
    single simulation or multiple simulations if n_reps>1. 
    Note that here we are
    simply building the simulation by modifying the base sim. Running the sims
    and extracting results will be done by the calibration function.
    """

    sir = sim.pars.diseases # The disease in this simulation and it is a SIR

    for k, pars in calib_pars.items(): # Loop over the calibration parameters
        if k == 'rand_seed':
            sim.pars.rand_seed = v
            continue

        # Each item in calib_pars is a dictionary with keys like 'low', 'high',
        # 'guess', 'suggest_type', and importantly 'value'. The 'value' key is
        # the one we want to use as that's the one selected by the algorithm
        v = pars['value']
        if k == 'beta':
            # this line seems broken as written so updated it
            sir['sir']['beta'] = ss.perday(v)
        # elif k == 'init_prev':
        #     sir.pars.init_prev = ss.bernoulli(v)
        else:
            raise NotImplementedError(f'Parameter {k} not recognized')

    # i think insert a shrink step here to override the manual one?
    # sim.Sim.shrink(
    
    # If just one simulation per parameter set, return the single simulation
    if n_reps == 1:
        return sim

    # But if you'd like to run multiple simulations with the same parameters, 
    # we return a MultiSim instead
    # Note that each simulation will have a different random seed, 
    # you can set specific seeds if you like
    # Also note that parallel=False and debug=True are important to avoid 
    # issues with parallelism in the calibration
    # Advanced: If running multiple reps, you can choose if/how they are 
    # combined using the "combine_reps" argument to each CalibComponent, introduced below.
    ms = ss.MultiSim(
        sim, 
        iterpars=dict(rand_seed=np.random.randint(0, 1e6, n_reps)),
        initialize=True, 
        debug=True, 
        parallel=False)
    return ms



sc.heading('Beginning calibration')

# Make the sim and data
sim = make_sim()

# Make the calibration
calib = ss.Calibration(
    calib_pars = calib_pars,
    sim = sim,
    build_fn = build_sim,
    build_kw = dict(n_reps=3), # Run 3 replicates for each parameter set
    reseed = True, # If true, a different random seed will be provided to each configuration
    total_trials = 100, # Use more for a real calibration
    n_workers = 1, # None indicates to use all available CPUs
    die = True,
    debug = False, # Run in serial if True
    verbose = 0
)

# Perform the calibration
sc.printcyan('\nPerforming calibration...')
calib.calibrate();