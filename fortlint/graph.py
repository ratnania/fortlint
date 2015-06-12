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
    def __init__(self, key, label=None, color=None):
        self._ID  = id(key)
#        print "*** node created with id:", self.ID
        self._key = key
        self._connectedTo = {}
        self._color = color
        self._label = label

        try:
            self._label = self.key.label
        except:
            if label is None:
                self._label = self.ID
            else:
                self._label = label

        try:
            self._color = self.key.color
        except:
            if color is None:
                self._color = "black"
            else:
                self._color = color

    @property
    def ID(self):
        return self._ID

    @property
    def key(self):
        return self._key

    @property
    def color(self):
        return self._color

    @property
    def label(self):
        return self._label

    @property
    def connectedTo(self):
        return self._connectedTo

    def addNeighbor(self, vertex, weight=0, constraint="false", style="solid"):
        """
        TODO : insert constraint in data structure
        """
        data = {}
        data['weight']     = weight
        data['constraint'] = constraint
        data['style']      = style
        if vertex not in self.connectedTo:
            self._connectedTo[vertex] = data

    def __str__(self):
#        return str(self.ID) + ' connectedTo: ' + str([x.ID for x in self.connectedTo])
        return ""

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

    def edge(self, vertex_f, vertex_t, \
             weight=0, constraint="false", style="solid"):
        if vertex_f not in self.nodes:
#            print ("create new vertex_f :", vertex_f)
            try:
                self.node(vertex_f, label=vertex_f.label)
            except:
                self.node(vertex_f, label=vertex_f)

        if vertex_t not in self.nodes:
#            print ("create new vertex_t :", vertex_t)
            try:
                self.node(vertex_t, label=vertex_t.label)
            except:
                self.node(vertex_t, label=vertex_t)

        self._nodes[vertex_f].addNeighbor(self.nodes[vertex_t], \
                                          weight=weight, \
                                          constraint=constraint, \
                                          style=style)

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
#                print (node.ID, node.label, node.color)
                dot.node(str(node.ID), \
                         label=str(node.label), \
                         color=str(node.color))

            for node in self:
                for key, values in node.connectedTo.items():
#                for son in node.getConnections():

                    son = key
                    attr = values
#                    print ("XXXXX son :", son, " attr :", attr)
                    dot.edge(str(node.ID), str(son.ID), \
                             constraint=attr["constraint"], \
#                             weight=attr["weight"] \
                             style=attr["style"] )
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

