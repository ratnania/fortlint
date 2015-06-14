# -*- coding: utf8 -*-

from fortlint.graph import *

PRINT = False
#PRINT = True

# ...
def test_1():
    if PRINT :
        print ">>> test_1: Begin"

    g = Graph()
    for i in range(6):
        g.node(i)

    g.edge(0, 1, attributs={'weight':5})
    g.edge(0, 5, attributs={'weight':2})
    g.edge(1, 2, attributs={'weight':4})
    g.edge(2, 3, attributs={'weight':9})
    g.edge(3, 4, attributs={'weight':7})
    g.edge(3, 5, attributs={'weight':3})
    g.edge(4, 0, attributs={'weight':1})
    g.edge(5, 4, attributs={'weight':8})
    g.edge(5, 2, attributs={'weight':1})

    if PRINT :
        for v in g:
            for w in v.getConnections():
                print("( %s , %s )" % (v.ID, w.ID))
        print "<<< test_1: End"
# ...

# ...
def test_2():
    if PRINT :
        print ">>> test_2: Begin"

    g = Graph()
    for i in "abcdef":
        g.node(i)

    g.edge("a", "b", attributs={'weight':5})
    g.edge("a", "f", attributs={'weight':2})
    g.edge("b", "c", attributs={'weight':4})
    g.edge("c", "d", attributs={'weight':9})
    g.edge("d", "e", attributs={'weight':7})
    g.edge("d", "f", attributs={'weight':3})
    g.edge("e", "a", attributs={'weight':1})
    g.edge("f", "e", attributs={'weight':8})
    g.edge("f", "c", attributs={'weight':1})

    if PRINT :
        for v in g:
            for w in v.getConnections():
                print("( %s , %s )" % (v.ID, w.ID))
        print "<<< test_2: End"
# ...

# ...
#def test_3():
#    if PRINT :
#        print ">>> test_3: Begin"
#
#    g = Digraph()
#    for i in "abcdef":
#        g.node(i, attributs={'label':str(i)})
#
#    g.edge("a", "b", attributs={'weight':5})
#    g.edge("a", "f", attributs={'weight':2})
#    g.edge("b", "c", attributs={'weight':4})
#    g.edge("c", "d", attributs={'weight':9})
#    g.edge("d", "e", attributs={'weight':7})
#    g.edge("d", "f", attributs={'weight':3})
#    g.edge("e", "a", attributs={'weight':1})
#    g.edge("f", "e", attributs={'weight':8})
#    g.edge("f", "c", attributs={'weight':1})
#
#    # ... convert to graphviz
#    dot = g.to_graphviz()
#    dot.render('test_3-dot.gv', view=False)
#    # ...
#
#    g.render('test_3-g.gv', view=False)
#
#    if PRINT :
#        for v in g:
#            for w in v.getConnections():
#                print("( %s , %s )" % (v.ID, w.ID))
#        print "<<< test_3: End"
# ...

# ...
#def test_4():
#    if PRINT :
#        print ">>> test_4: Begin"
#    # ...
#    class Node(object):
#        def __init__(self, a, b):
#            self.a = a
#            self.b = b
#
#        @property
#        def label(self):
#            return self.a + self.b
#    # ...
#
#    g = Digraph()
#    node_a = Node("2","3")
#    node_b = Node("1","5")
#    node_c = Node("3","4")
#    node_d = Node("4","2")
#    node_e = Node("3","2")
#    node_f = Node("1","2")
#    g.node(node_a, node_a.label)
#    g.node(node_b, node_b.label)
#    g.node(node_c, node_c.label)
#    g.node(node_d, node_d.label)
#    g.node(node_e, node_e.label)
#    g.node(node_f, node_f.label)
#
#    g.edge(node_a, node_b, attributs={'weight':5})
#    g.edge(node_a, node_f, attributs={'weight':2})
#    g.edge(node_b, node_c, attributs={'weight':4})
#    g.edge(node_c, node_d, attributs={'weight':9})
#    g.edge(node_d, node_e, attributs={'weight':7})
#    g.edge(node_d, node_f, attributs={'weight':3})
#    g.edge(node_e, node_a, attributs={'weight':1})
#    g.edge(node_f, node_e, attributs={'weight':8})
#    g.edge(node_f, node_c, attributs={'weight':1})
#
#    # ... convert to graphviz
#    dot = g.to_graphviz()
#    dot.render('test_4-dot.gv', view=False)
#    # ...
#
#    g.render('test_4-g.gv', view=False)
#
#    if PRINT :
#        for v in g:
#            for w in v.getConnections():
#                print("( %s , %s )" % (v.ID, w.ID))
#        print "<<< test_4: End"
# ...



###############################################################
if __name__ == "__main__":
    test_1()
    test_2()
#    test_3()
#    test_4()
