# stated vs revealed preferences
stated vs revealed preferences and agent-based modelling

## ABM2104.r
Contains agent-based modelling r script. This simulation involves male and female agents (simulated participants) rating opposite-sex agents according to the constraints of the real-life speed-dating environment. ABM data has been generated in the same structure as the real-life speed-dating data so that MLM could be performed and direct comparisons can be made between the γ obtained from simulated and real-life speed-dating data.
### Variable definitions
* Agent_i: Denotes the specific individual who gives the rating within the simulation, this is given by their ID number {1000, 1001, …}.
* Partner_j: Denotes another agent j who receives trait, trait appeal, and overall attractiveness ratings by an agent i, this is given by their ID number {1000, 1001, …}.
* Trait_k: Denotes the kth trait, where k ranges from 1 to n (the maximum number of traits used to determine overall attractiveness in the simulation). Examples of traits may be facial attractiveness, intelligence, confidence etc. It is assumed that all traits are independent of each other. 
* Latent trait score_jk: The extent to which partner j possesses a certain trait k on a scale from 1 = Well below average to 7 = Well above average. Each agent’s latent trait value has been sampled from a normal distribution (M = 4.00, SD = 1.50).
* Stated trait preference_ik: The extent that agent i believes it is important for an ideal partner to possess trait k on a scale from 1 = Not at all to 7 = Extremely important. Each agent’s preference value has been sampled from a normal distribution where male and female parameters have been obtained from the speed-dating data (M = 5.314, SD = 1.195, and M = 5.346, SD = 1.215, respectively)
* Rating bias_i: Each agent i may not accurately perceive the extent to which a partner possesses a certain trait and therefore an individual may systematically over or underestimate their rating. This bias has been sampled from a normal distribution (M = 0.00, SD = 1.50).

### Parameters
The simulation has several parameters that may be changed to suit different scenarios.
* The number of traits used by each agent to determine overall attractiveness, varying from 2 to 30 
* There were 171 speed-dating sessions used as per the maximum number of sessions in the real-life speed-dating data
* There were a minimum of 2 and maximum of 5 male and female participants per session which were normally distributed as per the real-life speed-dating data (males: M = 3.582, SD = 0.902, females: M = 3.693, SD = 0.839) 
A noise term which disrupts the extent to which stated preferences inform how appealing a partner’s trait is; ranges from 0 to 50. A noise value of 0 would result in stated preferences completely informing the extent to which a participant finds a partner’s trait appealing.
### Generation of speed-dating interactions
The number of males and females per speed-dating session were generated according to parameters obtained from the real speed-dating data. These values were then rounded to the nearest integer and were restricted between 2 to 5 as per the real-life speed-dating study. Participant IDs were generated for each individual where males had IDs starting from 1000 and female IDs starting from 2000. All possible combinations for each agent and partner were generated for each session and ‘Interaction’ is the order that the participant meets their partner.
### Assigning participant traits
Each agent was assigned latent trait scores, a rating bias, and stated partner preferences. For the specified number of traits, latent trait scores were generated according to a normal distribution (M = 4.00, SD = 1.50) and stated partner preferences were generated for each trait according to the distribution of preferences from the speed-dating data and rounded to the nearest integer between 1 and 7. A rating bias was generated for each individual according to a normal distribution (M = 0.00, SD = 1.50). 

