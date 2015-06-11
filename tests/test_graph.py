# -*- coding: utf8 -*-

from fortlint.graph import *

# ...
def test_1():
    print ">>> test_1: Begin"

    g = Graph()
    for i in range(6):
        g.node(i)

    g.edge(0, 1, weight=5)
    g.edge(0, 5, weight=2)
    g.edge(1, 2, weight=4)
    g.edge(2, 3, weight=9)
    g.edge(3, 4, weight=7)
    g.edge(3, 5, weight=3)
    g.edge(4, 0, weight=1)
    g.edge(5, 4, weight=8)
    g.edge(5, 2, weight=1)
    for v in g:
        for w in v.getConnections():
            print("( %s , %s )" % (v.ID, w.ID))

    print "<<< test_1: End"
# ...

# ...
def test_2():
    print ">>> test_2: Begin"

    g = Graph()
    for i in "abcdef":
        g.node(i)

    g.edge("a", "b", weight=5)
    g.edge("a", "f", weight=2)
    g.edge("b", "c", weight=4)
    g.edge("c", "d", weight=9)
    g.edge("d", "e", weight=7)
    g.edge("d", "f", weight=3)
    g.edge("e", "a", weight=1)
    g.edge("f", "e", weight=8)
    g.edge("f", "c", weight=1)
    for v in g:
        for w in v.getConnections():
            print("( %s , %s )" % (v.ID, w.ID))

    print "<<< test_2: End"
# ...

# ...
def test_3():
    print ">>> test_3: Begin"

    g = Digraph()
    for i in "abcdef":
        g.node(i, label=i)

    g.edge("a", "b", weight=5)
    g.edge("a", "f", weight=2)
    g.edge("b", "c", weight=4)
    g.edge("c", "d", weight=9)
    g.edge("d", "e", weight=7)
    g.edge("d", "f", weight=3)
    g.edge("e", "a", weight=1)
    g.edge("f", "e", weight=8)
    g.edge("f", "c", weight=1)
    for v in g:
        for w in v.getConnections():
            print("( %s , %s )" % (v.ID, w.ID))

    # ... convert to graphviz
    dot = g.to_graphviz()
    dot.render('test_3-dot.gv', view=False)
    # ...

    g.render('test_3-g.gv', view=False)

    print "<<< test_3: End"
# ...

# ...
def test_4():
    print ">>> test_4: Begin"
    # ...
    class Node(object):
        def __init__(self, a, b, color="blue"):
            self.a = a
            self.b = b
            self._color = color

        @property
        def label(self):
            return self.a + self.b

        @property
        def color(self):
            return self._color
    # ...

    g = Digraph()
    node_a = Node("2","3")
    node_b = Node("1","5")
    node_c = Node("3","4")
    node_d = Node("4","2")
    node_e = Node("3","2")
    node_f = Node("1","2")
    g.node(node_a, node_a.label)
    g.node(node_b, node_b.label)
    g.node(node_c, node_c.label)
    g.node(node_d, node_d.label)
    g.node(node_e, node_e.label)
    g.node(node_f, node_f.label)

    g.edge(node_a, node_b, weight=5)
    g.edge(node_a, node_f, weight=2)
    g.edge(node_b, node_c, weight=4)
    g.edge(node_c, node_d, weight=9)
    g.edge(node_d, node_e, weight=7)
    g.edge(node_d, node_f, weight=3)
    g.edge(node_e, node_a, weight=1)
    g.edge(node_f, node_e, weight=8)
    g.edge(node_f, node_c, weight=1)

    for v in g:
        for w in v.getConnections():
            print("( %s , %s )" % (v.ID, w.ID))

    # ... convert to graphviz
    dot = g.to_graphviz()
    dot.render('test_4-dot.gv', view=False)
    # ...

    g.render('test_4-g.gv', view=False)

    print "<<< test_4: End"
# ...



###############################################################
if __name__ == "__main__":
#    test_1()
#    test_2()
#    test_3()
    test_4()
