from sweat import read_fit

def read_fit_file(f):
  d = read_fit(f)
  d = d.reset_index()
  return d
