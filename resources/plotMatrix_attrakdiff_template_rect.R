# Settings for matrix rectangles
attrakdiff_rect <- rbind(
  tl = data.frame(xmin = -3, xmax = -1, ymin =  1, ymax =  3, col = "grey70"),
  tm = data.frame(xmin = -1, xmax =  1, ymin =  1, ymax =  3, col = "grey85"),
  tr = data.frame(xmin =  1, xmax =  3, ymin =  1, ymax =  3, col = "grey85"),
  ml = data.frame(xmin = -3, xmax = -1, ymin = -1, ymax =  1, col = "grey55"),
  mm = data.frame(xmin = -1, xmax =  1, ymin = -1, ymax =  1, col = "grey70"),
  mr = data.frame(xmin =  1, xmax =  3, ymin = -1, ymax =  1, col = "grey85"),
  bl = data.frame(xmin = -3, xmax = -1, ymin = -3, ymax = -1, col = "grey40"),
  bm = data.frame(xmin = -1, xmax =  1, ymin = -3, ymax = -1, col = "grey55"),  
  br = data.frame(xmin =  1, xmax =  3, ymin = -3, ymax = -1, col = "grey70"))

# Settings for matrix labels
labels_attrakdiff_matrix <- rbind(
  tl = data.frame(x = -2, y =  2, 
                  label_ger = "zu\nselbst-\norientiert", 
                  label_eng = "too\nself-\noriented"),
  tm = data.frame(x =  0, y =  2, 
                  label_ger = "selbst-\norientiert", 
                  label_eng = "self-\noriented"),
  tr = data.frame(x =  2, y =  2, 
                  label_ger = "begehrt", 
                  label_eng = "desired"),
  mm = data.frame(x =  0, y =  0, 
                  label_ger = "neutral", 
                  label_eng = "neutral"),
  mr = data.frame(x =  2, y =  0, 
                  label_ger = "handlungs-\norientiert", 
                  label_eng = "task-\noriented"),
  bl = data.frame(x = -2, y = -2, 
                  label_ger = "überflüssig", 
                  label_eng = "superfluous"),
  br = data.frame(x =  2, y = -2, 
                  label_ger = "zu\nhandlungs-\norientiert", 
                  label_eng = "too\ntask-\noriented"))