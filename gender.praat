form: ""
  sentence: "wave file", ""
  sentence: "temp path", ""
  
  choice: "removesilence", 1
    option: "yes"
    option: "no"

  choice: "changegender", 1
    option: "yes"
    option: "no"
endform

Read from file: wave_file$
Rename: "wavefile"

if removesilence$="yes"
  To TextGrid (speech activity): 0, 0.3, 0.1, 70, 6000, -10, -35, 0.1, 0.1, "", "speech"
  Rename: "textgrid"

  selectObject: "TextGrid textgrid"
  Get starting points: 1, "is equal to", "speech"
  starttime = Get time from index: 1

  selectObject: "TextGrid textgrid"
  Get end points: 1, "is equal to", "speech"
  nop = Get number of points
  endtime = Get time from index: nop

  selectObject: "Sound wavefile"
  Extract part: starttime, endtime, "rectangular", 1, "no"

  selectObject: "Sound wavefile"
  Remove

  selectObject: "Sound wavefile_part"
  Rename: "wavefile"
endif

if changegender$="yes"
  To Pitch: 0, 75, 600
  pitch_mean  = Get mean: 0, 0, "Hertz"
  pitch_mean$ = Get mean: 0, 0, "Hertz"

  if pitch_mean$ == "--undefined-- Hz"
    pitch_mean = 0
  endif

  selectObject: "Sound wavefile"
  Change gender: 75, 600, 1.2, pitch_mean * 2.0, 1, 1

  selectObject: "Sound wavefile"
  Remove

  selectObject: "Sound wavefile_changeGender"
  Rename: "wavefile"
endif

selectObject: "Sound wavefile"
Save as WAV file: temp_path$ + "tmp.wav"
