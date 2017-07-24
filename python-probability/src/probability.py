from enum import Enum
import math


class Distribution:
    def __init__(self, df):
        """

        :param df: maps an element of the domain to a probability.
        """
        self.df = df  # density function


class Discrete(Distribution):

    @staticmethod
    def from_pairs(pairs):
        return Discrete(dict(pairs).__getitem__)
        # return Discrete(lambda x: dict(pairs)[x])


class ContinuousDistribution(Distribution):
    def __init__(self, pdf, cdf):
        super().__init__(pdf)
        self.pdf = pdf
        self.cdf = cdf


class Normal(Distribution):
    def __init__(self, mu, sigma):
        frac = 1 / (math.sqrt(2 * math.pi * sigma ** 2))
        normalpdf = lambda x: frac * ((- (x - mu) ** 2) / (2 * sigma ** 2))
        super().__init__(normalpdf)


class Element:
    def __init__(self):
        self.observed = False
        self.val = None

    def observe(self, val):
        self.observed = True
        self.val = val

    def unobserve(self):
        self.observed = False
        self.val = None


class Atomic(Element):
    """An element that has no parent.  It is specified with
    an explicit distribution.
    """
    def __init__(self, dist: Distribution):
        super().__init__()
        self.dist = dist


class CPD(Element):
    """An element, with a parent.  The element's distribution is a
    conditional probability distribution, conditioned on the parent's
    distribution.

    :param conditions a list of tuples (v, e), e being the element
    that is causally implied by a parent value of v.
    """
    def __init__(self, parent: Element, *args):
        super().__init__()
        assert isinstance(parent, Discrete)
        self.parent = parent
        print(args)
        self.conditions = args[0]


class MultipleCPD(Element):
    """Like CPD, except that there are multiple parents; the current
    element's distribution is conditioned on their joint distribution.

    We're dealing only with discrete distributions here.
    """
    def __init__(self, parents, *args):
        for parent in parents:
            assert isinstance(parent, Discrete)

        super().__init__()
        self.parents = parents
        self.conditions = args[0]


class If(CPD):
    def __init__(self, chk: Element, thn: Element, els: Element):
        super().__init__(chk, [(True, thn), (False, els)])


class Flip(Atomic):
    def __init__(self, prob):
        super().__init__(Discrete.from_pairs([(True, prob), (False, 1 - prob)]))


def probability(element, val):
    if element.observed:
        return 1 if element.val == val else 0

    # Element not observed, use the distributions.
    if isinstance(element, Atomic):
        return element.dist.df(val)
    elif isinstance(element, CPD):
        prob = 0
        for cond in element.conditions:
            # TODO(dhruv) this should always be the case.
            if isinstance(cond[1], Element):
                prob += probability(element.parent, cond[0]) * probability(cond[1], val)
            elif cond[1] == val:
                prob += probability(element.parent, cond[0])
        return prob
    elif isinstance(element, MultipleCPD):
        prob = 0
        for cond in element.conditions:
            if isinstance(cond[1], Element):
                prod = 1
                for i, value in enumerate(cond[0]):
                    prod *= probability(element.parents[i], value)
                prob += prod * probability(cond[1], val)
            elif cond[1] == val:
                prod = 1
                for value in cond[0]:
                    prod *= probability(element.parents, value)
                prob += prod
        return prob


def main():
    # dist = Distribution.from_pairs([(True, 0.4), (False, 0.6)])
    # x = Atomic(dist)
    # y = Atomic(Distribution.from_pairs([("sunny", 0.9), ("rainy", 0.1)]))
    # z = Atomic(Distribution.from_pairs([("sunny", 0.1), ("rainy", 0.1)]))
    # w = If(x, y, z)
    # x.observe(False)
    # print(probability(w, "sunny"))
    #
    # a = Atomic(dist)
    # dist1 = Distribution.from_pairs([("paris", 0.2), ("rome", 0.3), ("venice", 0.5)])
    # dist2 = Distribution.from_pairs([("paris", 0.5), ("rome", 0.2), ("venice", 0.3)])
    # c1 = Atomic(dist1)
    # c2 = Atomic(dist2)
    # b = CPD(a, [(True, c1), (False, c2)])
    # print(probability(b, "paris"))

    # Burglary example from chapter 14
    burglary = Flip(0.001)
    earthquake = Flip(0.002)
    alarm = MultipleCPD([burglary, earthquake], [([True, True], Flip(0.95)),
                                                 ([True, False], Flip(0.94)),
                                                 ([False, True], Flip(0.29)),
                                                 ([False, False], Flip(0.001))])
    john = If(alarm, Flip(0.9), Flip(0.05))
    mary = If(alarm, Flip(0.7), Flip(0.01))

    # burglary.observe(True)
    # earthquake.observe(True)
    print("burglary: ", probability(john, True))


if __name__ == '__main__':
    main()
