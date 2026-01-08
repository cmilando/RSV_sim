
# Jan 2026

Chad Milando

Thoughts on the RSV modeling project. 
TLDR is I think it makes sense to build the hybrid compartmental-agent based model
that we talked about today, using R and Rcpp to make the computations faster.


## Potential issues with starsim

(1) just 1 transmission probability for each simulation,
whereas we want a matrix (kids to kids, kids to adults, adults to kids etc )
* HPV is different males -> females and females -> males

(2) no way to change states within the day (home to school to home)

(3) and relatedly no way to have a 3-D contact matrix (so all-ages to all-ages in
school home or work etc)

## And so

We are going to build a hybrid compartment and agent-based model to get the things 
that we want

It will be a dynamic, Time-of-day and location
every age-group at every time simulation for RSV

there will be 4 age groups: 

* <5, 
* 5-18, 
* 19-64, 
* 65+

and 4 locations: 

* home, 
* school, 
* community, 
* and work

each person has a house id, a school id, a community it, and work id

and a time-activity pattern for where they are during the day

Each of these are poisson distributions so you can model with a single parameter.

## Calibration #1 - the contact matrix

So the way we will calibrate to the contact matrix is to have

known:

* poisson mu for househould size
* poisson mu for school size
* poisson mu for work size

unknown:

* poisson mu for community size

target:

* contact matrix 

need to figure out the correct units for p(exposure) but we have all the details we need

from this we'll know how large each community should be, and these will be randomly assigned
once

## Calibration #2 

what is the transmission probability? 

the way we get this is the # of particl / kg - this becomes the target

essentially we'd be tuning to what is the # particles/kg that results in sickness
and we have the prevalence #s

each of the spacaces has a volume property
each person has a breathing rate
the virus has a decay rate in time

## the loop

so the loop becomes a differential equation in each space of the viral load
but the sources becomes a function of the people there

so the loop looks like

```
for(space_i in all_spaces) {

 (1) what is the new source flux (how many sick people are in the room). 
The sink is just a decay rate

 (2) use Runga-kutta to calculate end-of-timestep concentration

 (3) give people the average of the starting and ending concentration, i.e., each person
"inhales" the room concentration at their own rate

 (4) enter the `person loop` to see what happens, but this is just updating the 
      state properties and time-activity for each person

 (5) apply the time-activity action for each person. this is the last thing that
      happens for each timestep

}
```

## the person function

Here is where things get interesting. 
the above step can't happen in parallel but this step can

```
for(person_i in all_people) {

 (1) is disease state morbidity?

 (2) what has your inhaled been in the last few hours, so have you accumulated enough load
  
 (3) if it has, enter the incubation period and then in later time-steps, update your self parameters to be reflective of the natural history course
 perhaps your time-activity changes. upper resp, lower resp etc
 see the flowchart in K
 
}
```
