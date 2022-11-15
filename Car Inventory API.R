## ---- car-inventory
library(ggplot2)
library(dplyr)
inventory <- read.csv("inventory.csv", stringsAsFactors = FALSE)

#* @apiTitle Excercise: Auto Inventory Manager
#* @apiDescription Discover the different applications of an API while managing the inventory of an automobile store.


#* List all cars in the inventory.
#* This will provide the user with all the data points in the inventory. 
#* @get /car/
#* @tag Cars
listCars <- function(){
  inventory
}

#*Find the cars in inventory by manufacturer.
#* Lookup cars by manufacturer (Options: Buick, Chevrolet, Ford, GMC, Nissan, Toyota, Volvo)
#* @param make The name of the manufacturer of the car 
#* @get /car/<make:character>
#* @response 404 No car with the given maker was found in the inventory.
#* @tag Cars
getCar <- function(make, res){
  car <- inventory[inventory$make == make,]
  if (nrow(car) == 0){
    res$status <- 404
  }
  car
}


#* Create a Scatter Plot 
#* See the relationship between mileage and year a car was manufactured.
#* @serializer png
#* @get /plot_scatter
#* @tag Cars
getChart2 <-  function() {
  c <- ggplot(inventory, aes(x=year, y=miles, color=make)) + 
    geom_point(size=6) +
    scale_y_continuous(name="Car Mileage", breaks= waiver(),limits=c(15000, 300000))+
    scale_x_continuous(name="Year", limits=c(1990, 2020))
  print(c)
}

#* Create a Frequency chart 
#* See how many cars are in the inventory from the different makers.
#* @serializer png
#* @get /plot_frequency
#* @tag Cars
getChart1 <- function() {
  b <- ggplot(inventory, aes(x=factor(make)))+
    geom_bar(stat="count", width=0.7, fill="steelblue", color= "black")+
    scale_y_continuous(name="Count")+
    xlab("Car Manufactures")+
    theme_minimal()
  print(b)
}


#* Average price per year
#* See what is the average selling price for cars by the year they were manufactured.
#* @serializer png
#* @get /plot_average
#* @tag Cars
getChart3 <-  function() {
  d <- ggplot(inventory, aes(year, price)) +          
    geom_bar(position = "dodge",
             stat = "summary",
             fun = "mean",
             width=0.7,
             fill="steelblue",
             color= "black") + 
    geom_point(size= 4) +
    theme_minimal()+
    scale_y_continuous(name="Price", breaks= waiver())+
    xlab("Year")
  print(d)
}




#* Add a car to the inventory
#* @post /car/
#* @param make:character The make of the car
#* @param model:character The model of the car
#* @param edition:character Edition of the car
#* @param year:int Year the car was made
#* @param miles:int The number of miles the car has
#* @param price:numeric The price of the car in USD
#* @response 400 Invalid user input provided
#* @tag Modify
addCar <- function(make, model, edition, year, miles, price, res){
  newId <- max(inventory$id) + 1

  valid <- validateCar(make, model, year)
  if (!is.null(valid)){
    res$status <- 400
    return(list(errors=paste0("Invalid car: ", valid)))
  }

  car <- list(
    id = newId,
    make = make,
    model = model,
    edition = edition,
    year = year,
    miles = miles,
    price = price
  )

  inventory <<- rbind(inventory, car)
  getCar(newId)
}

#* Update a car in the inventory
#* @param id:int The ID of the car to update
#* @param make:character The make of the car
#* @param model:character The model of the car
#* @param edition:character Edition of the car
#* @param year:int Year the car was made
#* @param miles:int The number of miles the car has
#* @param price:numeric The price of the car in USD
#* @put /car/<id:int>
#* @tag Modify
updateCar <- function(id, make, model, edition, year, miles, price, res){

  valid <- validateCar(make, model, year)
  if (!is.null(valid)){
    res$status <- 400
    return(list(errors=paste0("Invalid car: ", valid)))
  }

  updated <- list(
    id = id,
    make = make,
    model = model,
    edition = edition,
    year = year,
    miles = miles,
    price = price
  )

  if (!(id %in% inventory$id)){
    stop("No such ID: ", id)
  }

  inventory[inventory$id == id, ] <<- updated
  getCar(id)
}

#* Delete a car from the inventory
#* @param id:int The ID of the car to delete
#* @delete /car/<id:int>
#* @tag Modify
deleteCar <- function(id, res){
  if (!(id %in% inventory$id)){
    res$status <- 400
    return(list(errors=paste0("No such ID: ", id)))
  }
  inventory <<- inventory[inventory$id != id,]
}
