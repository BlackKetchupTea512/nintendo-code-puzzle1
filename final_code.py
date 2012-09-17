#!python

from first_code  import lets_take_tea_break
from second_code import Bars
from third_code  import encode_morse

try:
    from HTMLParser import HTMLParser
    from urllib2 import urlopen
except:
    from html.parser import HTMLParser
    from urllib.request import urlopen
    from functools import reduce

chars = " iTI"
Bars.xor = lambda self, i, x: self.__setitem__(i, chars[chars.index(self[i]) ^ chars.index(x)])
Bars.num = lambda self: reduce(lambda a,b: a*4+chars.index(b), self, 0)
verify_RSA_signature = lets_take_tea_break

class SignatureParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.verifing = 0
        self.e = 65537
        self.n = 47775743999999999999 # TODO: this bit length is too short for RSA!

    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)
        if self.verifing > 0:
            self.verifing += 1
        elif "data-sign" in attrs_dict:
            print("data-sign: " + attrs_dict["data-sign"])
            self.verifing = 1
            self.signature = int(attrs_dict["data-sign"])
            self.bars = Bars(" "*32)
            self.bars_pos = 0

    def handle_data(self, data):
        if self.verifing > 0:
            for c in data:
                c = c.upper()
                assert(ord(' ') < ord(c) and ord(c) < 127)
                code = encode_morse(c)
                for i in code:
                    self.bars.xor(self.bars_pos, i)
                    self.bars_pos += 1; self.bars_pos %= len(self.bars)
                self.bars_pos += 1; self.bars_pos %= len(self.bars)
                print(self.bars)
                self.bars.next()
                print(self.bars)

    def handle_endtag(self, tag):
        if self.verifing > 0:
            self.verifing -= 1
            if self.verifing == 0:
                hash_value = self.bars.num()
                if verify_RSA_signature(hash_value, self.e, self.n, self.signature):
                    print("verification succeeded!")
                else:
                    print("verification failed")


if __name__ == '__main__':
    import sys
    response = urlopen(sys.argv[1])
    parser = SignatureParser()
    parser.feed(response.read().decode('utf-8'))
    parser.close()


# pow(a, b) % n == powerMod(a, b, n)
# def powerMod(a, b, n):
#     t = 1
#     while b > 0:
#         if b & 1 == 1:
#             t = (t * a) % n
#         a = (a * a) % n
#         b >>= 1
#     return t
