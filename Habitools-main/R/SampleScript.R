# sample script
am_i_a_dog <- function(x){
    
    checkmate::assert_character(x)

    if(x == "dog") {
        print("i am a dog")
    }
    else {
        print("i am not a dog")
    }
}
