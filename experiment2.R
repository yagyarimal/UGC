#Flags
FLAGS=flags(flag_integer('dense_units1',32),
            flag_integer('dense_units2',16),  
            flag_numeric('dropout1',0.1),
            flag_numeric('dropout2',0.1), 
            flag_integer('batch_size',32))
 model =keras_model_sequential() %>% 
  layer_dense(units=FLAGS$dense_units1 ,#32
              activation = 'relu',
              input_shape = c(14)) %>%  #21 independent varialbes
  layer_dropout(rate=FLAGS$dropout1) %>%    # 0.1for avoiding  10 %overfittting
  layer_dense(units=FLAGS$dense_units2,        # 16 anohter dense layer
              activation = 'relu') %>% 
  layer_dropout(rate=FLAGS$dropout2) %>%      #0.1
  
  layer_dense(units=7,activation='softmax')   # our response is 3 units layers
#compile
model %>% compile(loss='categorical_crossentropy',# for more than two layers
                  optimizer='adam',
                  metrics='accuracy')
#fit
history=model %>%
      fit(training,
      trainlabels,
      epochs=50,
      batch_size=FLAGS$batch_size,   #  32    
      validation_split=0.2) # 20%
  