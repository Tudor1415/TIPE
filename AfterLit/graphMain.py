import os, sys
if 'SUMO_HOME' in os.environ:
    tools = os.path.join(os.environ['SUMO_HOME'], 'tools')
    sys.path.append(tools)
else:   
    sys.exit("please declare environment variable 'SUMO_HOME'")

# sumoBinary = "C:/Program Files (x86)/Eclipse/Sumo"
sumoBinary = "/usr/bin/sumo"
sumoConfig = ["-c", "sumo.sumocfg", "-S"]
sumoCmd = [sumoBinary, sumoConfig[0], sumoConfig[1], sumoConfig[2]]

import traci
import traci.constants as tc
import sumolib
import json

# STATE LISTENER CLASS
class StateListener(traci.StepListener):
    def __init__(self, vehicleIds, emergencyBreakThreshold=-4.0):
        self.vehicleIds = vehicleIds
        self.vehicles = {}
        self.collision = False

    def step(self, t=0):
        self.checkCollision()
        # indicate that the state publisher should stay active in the next step
        return True
    
    def checkCollision(self):
        # if SUMO detects a collision (e.g. teleports a vehicle) set the collision flag
        if (traci.simulation.getStartingTeleportNumber() > 0):
            print("\nCollision occured...")
            self.collision = True


# MAIN PROGRAM

print("Starting the TraCI server...")
traci.start(sumoCmd) 


step = 0
timeline = []
max_steps = 1000
while step < max_steps:
    # advance the simulation
    sys.stdout.write("\r Processing output, step : %d %s %s |" % (step, (step*50 // max_steps)* ".", (50 - step*50 // max_steps)* " "))
    sys.stdout.flush()
    traci.simulationStep()
    if step > 100:
        vehs = traci.vehicle.getIDList()
        timeline.append({})
        for v in vehs:
            timeline[-1][v] = (traci.vehicle.getSpeed(v), traci.vehicle.getPosition(v))
    step += 1


print("\nStopping the TraCI server...")

data = {}
for step, state in enumerate(timeline):
    data[step] = state

# Serializing json
json_object = json.dumps(data, indent=4)

# Writing to sample.json
with open("data.json", "w") as outfile:
    outfile.write(json_object)

traci.close()