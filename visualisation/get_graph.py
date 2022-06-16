import networkx as nx
import matplotlib.pyplot as plt

G_weighted = nx.Graph()

def discretize_graph(G, node1, node2, coef):
    G_weighted.add_edge(node1,f"{node1}_{node2}_0", weight=1)  

    G_weighted.nodes[node1]['weight'] = 1000
    G_weighted.nodes[f"{node1}_{node2}_0"]['weight']=1 

    for i in range(coef):
        G.add_edge(f"{node1}_{node2}_{i}",f"{node1}_{node2}_{i+1}", weight=1)
        G.nodes[f"{node1}_{node2}_{i}"]['weight'] = 1

    G_weighted.add_edge(f"{node1}_{node2}_{coef}", node2, weight=1)  

    G_weighted.nodes[node2]['weight'] = 1000
    G_weighted.nodes[f"{node1}_{node2}_{coef}"]['weight']=1     

discretize_graph(G_weighted, "A", "B", 1)

plt.figure(figsize=(10,10))

edge_weight = list(nx.get_edge_attributes(G_weighted,'weight').values())
node_weight = list(nx.get_node_attributes(G_weighted,'weight').values())

nx.draw_networkx(G_weighted, width=edge_weight, node_size=node_weight)
plt.show()