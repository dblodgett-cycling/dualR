from fitparse import FitFile

def get_fit_meta(f):
  fitfile = FitFile(f)

  out = {}
  
  for record in fitfile.get_messages("file_id"):

    for r in record:
      out[r.name] = r.value

  return out
