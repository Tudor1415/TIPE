import networkx as nx
import matplotlib.pyplot as plt

G_weighted = nx.Graph()

G_weighted.add_edge('abc','lifestyle', weight=5)
G_weighted.add_edge('abc','livestream', weight=3)
G_weighted.add_edge('abc','sony', weight=1)

G_weighted.nodes['abc']['weight'] = 500
G_weighted.nodes['lifestyle']['weight'] = 900
G_weighted.nodes['livestream']['weight'] = 700
G_weighted.nodes['sony']['weight'] = 1100

plt.figure(figsize=(10,10))

edge_weight = list(nx.get_edge_attributes(G_weighted,'weight').values())
node_weight = list(nx.get_node_attributes(G_weighted,'weight').values())

nx.draw_networkx(G_weighted, width=edge_weight, node_size=node_weight)
plt.show()