#Reduction of the dataset

#WEATHER CONDITIONS

the data about weather have a stroooong correlation (>0.99); 
we can keep just one of them as a summary of the information

#PHYSICAL DIMENSIONS

length and width correlated (0.86), even length and wheelbase (0.9)
and length and GVWR (0.9); so we can just jeep length as a summary of all these variables


in all these plots we see that the correlation is quite strong till an extremum 
(that is the group with max.payload>800). Checking we see that the group is composed only of vans, and we can see it because those are the vehicles with Consumption higher than 250

so we keep as a summary of the physical quantities Length, Height, Volume and max payload,
that seems to be good to describe basically all the vehicle 

#ALL THE OTHERS

I delete covariates link and availability (useless)

we check all remaining covariates at once (excluding the categorical ones);
we see clearly that Combined...Mild.Weather, Eletric.Range and Battery Capacity
are basically the same thing

we also delete comsumption (same as fuel.equivalent)

we also delete charge.power (same as charge.speed)

total power summarizes top speed and total torque very well

also acceleration can be deleted, because basically 
(inversely) proportional to total power, as the intuition would suggest

finally, the seats covariate is highly correlated to some physical features;
for the moment however we keep it pecause of its importance and peculiarity

