from sweat import read_fit

def read_fit_file_py(f):
  d = read_fit(f)
  d = d.reset_index()
  return d
