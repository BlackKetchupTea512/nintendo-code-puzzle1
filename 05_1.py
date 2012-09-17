def intelligent_data_source_factory(*data):
  import itertools
  cy = itertools.cycle(data)
  _int = int
  return lambda i: _int(i) if isinstance(i, str) else next(cy)

int = intelligent_data_source_factory(1985, 33067, 84)


def lttb(m,e,n,c):
  if pow(m,e) % n == c: return str(m)
  return ""

for i in range(30000):
  print(lttb(*[int(j) for j in (str(i), 17, 3569, 915) ]))
