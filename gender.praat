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
  To TextGrid (speech activity, Silero): 0.5, 0.1, 0.25, 0.03, "", "speech"
  Rename: "textgrid"

  n = Get number of intervals: 1

  hasSpeech = 0

  for i from 1 to n
    label$ = Get label of interval: 1, i

    if label$ = "speech"
      hasSpeech = 1
    endif
  endfor

  if hasSpeech = 1
    selectObject: "Sound wavefile"
    plusObject: "TextGrid textgrid"
  
    Extract intervals where: 1, "no", "is equal to", "speech"
    Concatenate

    selectObject: "Sound wavefile"
    Remove

    selectObject: "Sound chain"
    Rename: "wavefile"
  endif
endif

if changegender$="yes"
  selectObject: "Sound wavefile"

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
