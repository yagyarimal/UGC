#Flags
FLAGS= flags(flag_integer('dense_units',32)) #hidden leayer 32

model =keras_model_sequential() %>% 
              layer_dense(units=FLAGS$dense_units,
              activation = 'relu',
              input_shape = c(14)) %>%       #21 independent varialbes
              layer_dropout(rate=0.1) %>%    # 0.1for avoiding  10 %overfittting
              layer_dense(units=16,          # 16 anohter dense layer
              activation = 'relu') %>% 
              layer_dropout(rate=0.1) %>% #0.1
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
      batch_size=32,   #  32 
      validation_split=0.2) # 20%
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

