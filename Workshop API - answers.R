## ---- car-inventory
library(ggplot2) #Library to plot
library(dplyr) #Library for Data Wrangling
inventory <- read.csv("inventory.csv", stringsAsFactors = FALSE)

# Create Title of API -----------------------------------------------------

#In this section we will use @apiTitle to add the Title of our API and @apiDescription to add a small description. 
#Remember : -"#" is used to add comments to the Code
#           -"#*" is used for Plumber Comments
#           -"@" is used to define Plumber characteristics 

#* @apiTitle Excercise: Used American Automobiles Inventory Manager
#* @apiDescription Discover the different applications of an API while managing the inventory of an automobile store.

#* @apiTag General Functionality having to do with the management of the API.
#* @apiTag Cars Functionality having to do with the understanding of different variables within the car inventory.
#* @apiTag Modify Functionality having to do with the management of car inventory.

# Check if the API is running correctly! ----------------------------------
#* API status check
#* This will check if the API is up and running correctly on our machine. 
#* @get /status-check
#* @tag General
status <- function(){
  list(
    status = "Working Perfectly",
    time = Sys.time()
  )
}

# Now that we know that the API is running correctly, let's set up some requests 
# A quick reminder of the different commands in the plumber pipeline:
# - @get : request a resource
# - @post : send data in body
# - @put : store/update data
# - @delete : delete resource
# - @head : no request 
# - @option : describe options
# - @patch : partial changes
# - @use : use all methods

# Create your first Plumber Pipeline --------------------------------------
#* List all cars in the inventory
#* This will provide the user with all the data points in the inventory. 
#* @get /car/
#* @tag Cars
listCars <- function(){
  inventory
}

#* Look up a car by ID
#* This will provide the user with all the information regarding a car. 
#* @param id The ID of the car to get
# ^ what parameter would you like to look for in your request
#* @get /car/<id:int>
#* @response 404 No car with the given ID was found in the inventory.
#* @tag Cars
getCar <- function(id, res){
  car <- inventory[inventory$id == id,]
  if (nrow(car) == 0){
    res$status <- 404
  }
  car
}

validateCar <- function(make, model, year){
  if (missing(make) || nchar(make) == 0){
    return("No make specified")
  }
  if (missing(model) || nchar(model) == 0){
    return("No make specified")
  }
  if (missing(year) || as.integer(year) == 0){
    return("No year specified")
  }
  NULL
}


# Let's Plot some information! --------------------------------------------

#* Count cars in inventory by manufacturer.
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

#* Observe cars by mileage and manufacturing year.
#* See the relationship between mileage and year a car was manufactured.
#* @serializer png
# ^In what format do we want to get our request?  @serializer (Options: JSON, png, csv, ...etc)
#* @get /plot_scatter
#* @tag Cars
getChart2 <-  function() {
  c <- ggplot(inventory, aes(x=year, y=miles, color=make)) + 
    geom_point(size=6) +
    scale_y_continuous(name="Car Mileage", breaks= waiver(),limits=c(0, 300000))+
    scale_x_continuous(name="Year", limits=c(1990, 2020))+
    labs(title = "Distribution of cars by mileage and manufacturing year", color = "Manufacturer")
  print(c)
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

#Try it for yourself! Create a bar chart to see for each car manufacturer the sum of the prices for the cars in stock, separated by year.

#* Total worth of the inventory by year and manufacturer
#* @serializer png
#* @get /plot_inventory_price
#* @tag Cars
getChart4 <-  function() {
  e <- ggplot(inventory, aes(x = year, y= price, fill = make)) +  
    geom_col() + 
    labs(x = "Year", y = "Manufacturer")
  print(e)
}



# Modify some information on the data set! --------------------------------
#* Add a car to the inventory
#* @post /car/
#* @param make:character The manufacturer of the car
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