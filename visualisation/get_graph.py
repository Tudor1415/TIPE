import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import seaborn.apionly as sns
import matplotlib.animation
import pandas as pd

df = pd.read_csv("output.csv").to_numpy()

G_weighted = nx.Graph()

def discretize_graph(G, node1, node2, coef, total_weight):
    G_weighted.add_edge(node1,f"{node1}_{node2}_0", weight=1)  

    G_weighted.nodes[node1]['weight'] = 3000
    G_weighted.nodes[f"{node1}_{node2}_0"]['weight']=1 

    for i in range(coef):
        G.add_edge(f"{node1}_{node2}_{i}",f"{node1}_{node2}_{i+1}", weight=1, length= total_weight/coef)
        G.nodes[f"{node1}_{node2}_{i}"]['weight'] = 1

    G_weighted.add_edge(f"{node1}_{node2}_{coef}", node2, weight=1)  

    G_weighted.nodes[node2]['weight'] = 3000
    G_weighted.nodes[f"{node1}_{node2}_{coef}"]['weight']=1 

n = 10
hubs = ["A", "B", "C"]
distance = np.array([[0,3000,4500],[2500,0,1500],[1000,4000,0]])

for idx,hub in enumerate(hubs):
    discretize_graph(G_weighted, hub, hubs[(idx+1) % len(hubs)],distance[idx][(idx+1) % len(hubs)]//n, distance[idx][(idx+1) % len(hubs)])

fig, ax = plt.subplots(figsize=(15,15))

edge_weight = list(nx.get_edge_attributes(G_weighted,'weight').values())
node_weight = list(nx.get_node_attributes(G_weighted,'weight').values())

pos = nx.circular_layout(G_weighted)
nx.draw_networkx(G_weighted, width=edge_weight, with_labels=False, node_size=node_weight, pos = pos)

print(G_weighted.nodes())

labels = {}    
for node in G_weighted.nodes():
    if node in hubs:
        #set the node name as the key and the label as its value 
        labels[node] = node

nx.draw_networkx_labels(G_weighted,pos,labels,font_size=20,font_color='w')

def is_car(vehicle):
    return not ((-1) in vehicle)
    
def compute_node_car(car):
    ax.clear()
    number = int(car[1]*car[3]//n)
    return f"{hubs[int(car[4])]}_{hubs[int(car[5])]}_{number}"

def compute_node_pad(pad):
    number = int((distance[pad[2]][pad[3]] - pad[1])//n)
    return f"{hubs[int(pad[2])]}_{hubs[int(pad[3])]}_{number}"

def update(num):
    vehiclelist = df[num][:-1].reshape(len(df[num] -1)//6, 6)
    nodes = [] 
    for vehicle in vehiclelist:
        if is_car(vehicle):
            nodes.append(compute_node_car(vehicle))
        else:
            nodes.append(compute_node_pad(vehicle))      

    null_nodes = nx.draw_networkx_nodes(G_weighted, pos=pos, nodelist=set(G_weighted.nodes()) - set(nodes), node_color="white",  ax=ax)
    query_nodes = nx.draw_networkx_nodes(G_weighted, pos=pos, nodelist=nodes, node_color="orange", ax=ax)
    
    ax.set_title(f"Ittération: {vehiclelist[-1]}", fontweight="bold")
    ax.set_xticks([])
    ax.set_yticks([])

ani = matplotlib.animation.FuncAnimation(fig, update, frames=6, interval=1000, repeat=True)
# plt.savefig("Graph.png", format="PNG")
# plt.show()