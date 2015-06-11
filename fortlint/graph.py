# -*- coding: utf8 -*-

# ...
try:
    from graphviz import Digraph as Digraph_graphviz
    GRAPH=True
except ImportError:
    GRAPH=False
# ...

# ...
class Node:
    def __init__(self, key, label="", color="black"):
        self._ID = key
        self._connectedTo = {}
        self._color = color
        self._label = label

    @property
    def ID(self):
        return self._ID

    @property
    def color(self):
        return self._color

    @property
    def label(self):
        return self._label

    @property
    def connectedTo(self):
        return self._connectedTo

    def addNeighbor(self, vertex, weight=0, constraint="false"):
        """
        TODO : insert constraint in data structure
        """
#        self._connectedTo[str(vertex)] = weight
        self._connectedTo[vertex] = weight

    def __str__(self):
        return str(self.ID) + ' connectedTo: ' + str([x.ID for x in self.connectedTo])

    def getConnections(self):
        return self.connectedTo.keys()

    def getWeight(self, vertex):
#        return self.connectedTo[str(vertex)]
        return self.connectedTo[vertex]
# ...

# ...
class Graph:
    def __init__(self, comment=""):
        self._nodes = {}
        self._numVertices = 0
        self._comment = comment

    @property
    def nodes(self):
        return self._nodes

    @property
    def comment(self):
        return self._comment

    @property
    def numVertices(self):
        return self._numVertices

    def node(self, *args, **kwargs):
        self._numVertices = self._numVertices + 1

        newNode = Node(*args, **kwargs)

        key = args[0]
        self._nodes[key] = newNode
        return newNode

    def getNode(self,n):
        if n in self.nodes:
            return self.nodes[n]
        else:
            return None

    def __contains__(self,n):
        return n in self.nodes

    def edge(self, vertex_f, vertex_t, weight=0, constraint="false"):
        if vertex_f not in self.nodes:
            self.node(vertex_f, label=vertex_f)
#            print ("vertex_f is not defined. Given:"+vertex_f)
#            raise()

        if vertex_t not in self.nodes:
            self.node(vertex_t, label=vertex_t)
#            print vertex_t
#            print ("vertex_t is not defined. Given:"+vertex_t)
#            raise()

        self._nodes[vertex_f].addNeighbor(self.nodes[vertex_t], \
                                          weight=weight, \
                                          constraint=constraint)

    def getVertices(self):
        return self.nodes.keys()

    def __iter__(self):
        return iter(self.nodes.values())
# ...

# ...
class Digraph(Graph):
    def __init__(self, *args, **kwargs):
        Graph.__init__(self, *args, **kwargs)

    def to_graphviz(self):
        dot = None
        if GRAPH:
            dot = Digraph_graphviz(comment=self.comment)
            for node in self:
                dot.node(node.ID, label=node.label, color=node.color)
                for son in node.getConnections():
                    dot.edge(node.ID, son.ID, constraint="true")
        else:
            print ("graphviz is not available on this machine.")

        return dot

    def render(self, filename, view=False):
        dot = self.to_graphviz()
        if dot is not None:
            dot.render(filename, view=view)
        else:
            print ("rendering is provided only by graphviz and is not available on this machine.")
# ...

