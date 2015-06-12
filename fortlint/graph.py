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
    def __init__(self, key, attributs={}):
        self._ID  = id(key)
#        print "*** node created with id:", self.ID, attributs
        self._key = key
        self._dict_attributs = attributs
        self._connectedTo = {}

    @property
    def ID(self):
        return self._ID

    @property
    def key(self):
        return self._key

    @property
    def label(self):
        try:
            value = self.attributs["label"]
        except:
            value = None

        if value is None:
            self.init_label()

    @property
    def color(self):
        try:
            value = self.attributs["color"]
        except:
            value = None

        if value is None:
            self.init_color()

    @property
    def connectedTo(self):
        return self._connectedTo

    @property
    def attributs(self):
        return self._dict_attributs

    def set_attribut(self, key, value):
#        print "++++++ set_attribut with :", key, value
        self._dict_attributs[key] = value

    def set_label(self, value):
        self.set_attribut("label", value)

    def set_color(self, value):
        self.set_attribut("color", value)

    def init_label(self):
        try:
            value = self.attributs["label"]
        except:
            value = None

        if value is None:
            self.set_label(self.key.label)

    def init_color(self):
        try:
            value = self.attributs["color"]
        except:
            value = None

        if value is None:
            try:
                self.set_color(self.key.color)
            except:
                self.set_color("black")

    def update_attributs(self):
        self.init_color()
        self.init_label()

    def addNeighbor(self, vertex, attributs={}):
        """
        """
        if vertex not in self.connectedTo:
            self._connectedTo[vertex] = attributs

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
             attributs={}):

        for vertex in [vertex_f, vertex_t]:
            if vertex not in self.nodes:
#                print "---- create new vertex :", vertex, vertex.label
                v_attributs = {}
                try:
                    v_attributs["label"] = vertex.label
                except:
                    v_attributs["label"] = vertex

                self.node(vertex, attributs=v_attributs)

        self._nodes[vertex_f].addNeighbor(self.nodes[vertex_t], \
                                          attributs=attributs)

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

                node.update_attributs()
#                print (node.ID, node.attributs)
                dot.node(str(node.ID), **node.attributs)

            for node in self:
                for key, values in node.connectedTo.items():
#                for son in node.getConnections():

                    son = key
                    attr = values
#                    print ("XXXXX son :", son, " attr :", attr)
                    dot.edge(str(node.ID), str(son.ID), **attr)
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

