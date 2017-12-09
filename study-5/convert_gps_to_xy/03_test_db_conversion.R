ggplot() + 
  ## Driver path
  geom_path(data = dat_adtf_raw %>% 
              filter(file_name %in% sample(dat_adtf_raw2_summary$file_name, 200)),
            aes(x = pos_x,
                y = pos_y,
                group = file_name),
            size = 1) + 
  
  ## Parking vehicle
  geom_point(data = data.frame(X=1.4132504272460938e+02, Y=-2.5997845458984375e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 
  geom_point(data = data.frame(X=1.0045167541503906e+02, Y=-2.7543286132812500e+02),
             aes(x = X,
                 y = Y),
             color= "blue") +
  
  ## Vehicle at intersection
  geom_point(data = data.frame(X=-4.3962503393970820e+02, Y=-4.2817026386490869e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 
  geom_point(data = data.frame(X=-3.8804388427734375e+02, Y=-4.3699484252929688e+02),
             aes(x = X,
                 y = Y),
             color= "blue") + 
  
  ## Preceding vehicle
  geom_point(data = data.frame(X=-6.1787453802710695e+02, Y=-5.2793164934619699e+02),
             aes(x = X,
                 y = Y),
             color= "red") +
  
  ## Crossing cyclist
  geom_point(data = data.frame(X=8.6991783142089844e+01, Y=-2.7988125610351562e+02),
             aes(x = X,
                 y = Y),
             color= "red") + 
  
  ## Evasing vehicle
  geom_point(data = data.frame(X=-5.0457767916741614e+02, Y=-4.8502175737738213e+02),
             aes(x = X,
                 y = Y),
             color= "red")