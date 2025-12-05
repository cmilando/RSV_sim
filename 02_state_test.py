import sciris as sc
import numpy as np
import starsim as ss
import starsim_examples as sse
import matplotlib.pyplot as plt

small = 100

def init_debut(module, sim, uids):
    # Test setting the mean debut age by sex, 16 for men and 21 for women.
    loc = np.full(len(uids), 16)
    loc[sim.people.female[uids]] = 21
    return loc

mf_pars = {
    'debut': ss.normal(loc=init_debut, scale=2),  # Age of debut can vary by using callable parameter values
}
sim_pars = {'networks': [ss.MFNet(**mf_pars)], 'n_agents': small}
gon_pars = {'beta': {'mf': [0.08, 0.04]}}
gon = sse.Gonorrhea(**gon_pars)

sim = ss.Sim(pars=sim_pars, diseases=[gon])
sim.init()
sim.run()

plt.figure()
plt.plot(sim.timevec, sim.results.gonorrhea.n_infected)
plt.title('Number of gonorrhea infections')

    
# Possible to initialize people with extra states, e.g. a geolocation
def geo_func(n):
    locs = [1,2,3]
    return np.random.choice(locs, n)
extra_states = [ss.FloatArr('geolocation', default=geo_func)]
ppl = ss.People(small, extra_states=extra_states)

# Possible to add a module to people outside a sim (not typical workflow)
ppl.add_module(sse.HIV())
ppl.to_df()
