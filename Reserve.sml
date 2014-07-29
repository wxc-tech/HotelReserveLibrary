signature ROOMDETAIL = 
sig
   val doubleAvailable : int
   val kingAvailable : int
   val queenAvailable : int
   val minnights : int option
   val occupancyLimit : int
end;

functor MakeHotel (structure Q : ROOMDETAIL) :
sig
   exception NotEnough
   exception ReplicateID
   exception NotExist
   exception MinNight
   exception MaxOccupant
   datatype roomconfig = Double | Queen | King
   type resrecord = {ID:int,FName:string,LName:string,DateIn:int,NumN:int,NumO:int,Config_Info:roomconfig}
   type ressys
   val empty : ressys
   val resrecord_fetch : int -> string -> string -> int -> int -> int -> roomconfig -> resrecord
   val restrictions : resrecord -> int
   val lookup_record_helper : int -> resrecord list -> bool
   val lookup_date_helper : int -> int -> int -> bool
   val reserve : ressys -> resrecord -> ressys
   val pred_equal : resrecord -> int -> bool
   val cancel_record_helper : (resrecord -> int -> bool) -> (resrecord list) -> int -> (resrecord list)
   val cancel : ressys -> int -> ressys
   val getInventory_helper : ressys -> roomconfig -> int -> int
   val getInventory : ressys -> roomconfig -> int -> int
   val getInventorySpan : ressys -> roomconfig -> int -> int -> bool
   val completedStays : ressys -> int -> int
   val remove_comp_helper : (resrecord list) -> int -> (resrecord list)
   val removeCompletedStays : ressys -> int -> ressys
   val guestQuantity : ressys -> int -> int
end = struct
   exception NotEnough
   exception ReplicateID
   exception NotExist
   exception MinNight
   exception MaxOccupant
   datatype roomconfig = Double | Queen | King
   type resrecord = {ID:int,FName:string,LName:string,DateIn:int,NumN:int,NumO:int,Config_Info:roomconfig}
   type ressys = {resrecord_list:resrecord list, total_num_tuple:(int*int*int)};
   
   (*empty indicates a hotel that no one reserves, so the record list is nil.*)
   val empty = {resrecord_list = nil, total_num_tuple = (Q.doubleAvailable,Q.queenAvailable,Q.kingAvailable)} : ressys;
  
   (*
     Give id, firstname, lastname, checkin-date, number of nights, number of
     occupants, room configure to the funtion, it will produce a corresponding 
     record.
   *)
   fun resrecord_fetch id fname lname datein numn numo config_info =
       {ID=id,FName=fname,LName=lname,DateIn=datein,NumN=numn,NumO=numo,Config_Info=config_info};
   
   (*
     Give a record to the function, if the number of occupants of this record
     extends the defined occupant limit, it will return 1, if the number of 
     reserved nights is smaller than the defined mininal number of reserved 
     night, it will return 2, if there is no error about this record, return 3
   *) 
   fun restrictions res_rec = 
      if (#NumO(res_rec : resrecord)) > Q.occupancyLimit
      then
      1
      else if valOf(Q.minnights) > (#NumN(res_rec))
      then
      2
      else
      3;

   (*
     Give a record id num and a record list to this function, if the record list 
     includes a record whose record id is equal to id num, return true or return 
     false.
   *)
   fun lookup_record_helper id_num [] = false
      |  lookup_record_helper id_num (recordunit :: resrecord_list : resrecord list) = (#ID(recordunit) = id_num) orelse (lookup_record_helper id_num resrecord_list);
   
   (*
     Give a check in date(res_checkin), the number of nights(res_numn), a date(res_date) 
     to the function, if res_date is in the range [res_checkin, res_checkin + res_numn -1], 
     return true, or return false.
   *)   
   fun lookup_date_helper res_checkin res_numn res_date = 
      if (res_numn = 0)
      then
         false
      else if (res_checkin = res_date)
      then
         true
      else 
         lookup_date_helper (res_checkin+1) (res_numn-1)  res_date;
         
   (*
     Give a hotel system, a room configure and a date to the function, 
     it will return the number of the room of the given configure that
     has been reserved at the given date in the hotel system.
   *)
   fun getInventory_helper res_sys res_config res_date = 
      if (#resrecord_list(res_sys) = nil)
      then
           0
      else if ((#Config_Info(hd(#resrecord_list(res_sys : ressys)))) = res_config)
      then
          if (lookup_date_helper (#DateIn(hd(#resrecord_list(res_sys)))) (#NumN(hd(#resrecord_list(res_sys)))) res_date)
          then
             1 + (getInventory_helper {resrecord_list=tl(#resrecord_list(res_sys)),total_num_tuple = (#total_num_tuple(res_sys))} res_config res_date) 
          else
             getInventory_helper {resrecord_list=tl(#resrecord_list(res_sys)),total_num_tuple = (#total_num_tuple(res_sys))} res_config res_date
      else
          getInventory_helper {resrecord_list=tl(#resrecord_list(res_sys)),total_num_tuple = (#total_num_tuple(res_sys))} res_config res_date;
 
   (*
     Give a hotel system, a room configure and a date to the function, 
     it will return the number of the left room of the given configure that
     has not been reserved at the given date in the hotel system.
   *)
   fun getInventory res_sys res_config res_date =      
      if (res_config = Double)
      then
      ((#1(#total_num_tuple(res_sys)))-(getInventory_helper res_sys res_config res_date))
      else if (res_config = Queen)
      then
      ((#2(#total_num_tuple(res_sys)))-(getInventory_helper res_sys res_config res_date)) 
      else
      ((#3(#total_num_tuple(res_sys)))-(getInventory_helper res_sys res_config res_date));


   (*
     Give a hotel system(res_sys), a room configure(res_config), a date(res_date),
     a number of nights(res_num_night) to the function, if the inventory is enough
     for all night, return true, or return false.
   *)
   fun getInventorySpan res_sys res_config res_date res_num_night = 
       if res_num_night = 0
       then
          true
       else if (getInventory res_sys res_config res_date) > 0
       then
          getInventorySpan res_sys res_config (res_date+1) (res_num_night-1)
       else
          false;
   
    (*
      Give a hotel system and a record, if the number of occupant of this record exceeds
      the defined occupant limit, raise MaxOccpant Exception, if the number of night of
      this record is smaller than the defined minimal number of reserved night, raise 
      MinNight Exception, if there is already a record in the hotel system which has the
      same id number with the give record, raise ReplicateID Exception,if there is already
      not enough room for the given record, raise NotEnough Exception. If the given record
      doesn't have the above exception, add it into the record list of the new hotel system,
      and return this new hotel system.
    *)
    fun reserve res_sys res_rec = 
    let
    in
      if ((restrictions res_rec) = 1)
      then
           raise MaxOccupant
      else if ((restrictions res_rec = 2))
      then
           raise MinNight
      else if (lookup_record_helper (#ID(res_rec)) (#resrecord_list(res_sys)))
      then    
          raise ReplicateID
      else if (not (getInventorySpan res_sys (#Config_Info(res_rec)) (#DateIn(res_rec)) (#NumN(res_rec))) )
      then
          raise NotEnough
      else
          {resrecord_list=(res_rec :: (#resrecord_list(res_sys))),total_num_tuple=(#total_num_tuple(res_sys))}
     end 
     handle
      MaxOccupant => (print("The number of occupant of this reserve exceeds the occupant limit for one room\n");res_sys)
      | MinNight => (print("The number of night of this reserve is less than the minimal number of night for one reserve\n");res_sys)
      | ReplicateID => (print("The id number of this reserve has already existed in the record list\n");res_sys)
      | NotEnough => (print("There is no enough inventory for this reserve\n");res_sys);
   
   (*
    Give a record and a id number to the function, the id number of the given record
    is same to the give id number, return true, or return false.
   *)
   fun pred_equal res_rec res_id = 
         if ((#ID(res_rec : resrecord)) = (res_id : int))
         then
           true
         else
           false;

   (*
    Give the function pred_equal(defined above), a record list(res_rec_list) and a id number(res_id) to the funtion,
    it will return a record list which has removed the record whose id number is equal to the given id number(res_id).
   *)
   fun cancel_record_helper pred_equal res_rec_list res_id = 
         if (res_rec_list = nil)
         then
            nil : resrecord list
         else if (pred_equal (hd(res_rec_list : resrecord list)) res_id)
         then   
            cancel_record_helper pred_equal (tl(res_rec_list)) res_id
         else
            ((hd(res_rec_list)) :: (cancel_record_helper pred_equal (tl(res_rec_list)) res_id));
 
   (*
    Give a hotel system(res_sys) and record id number(res_id) to the function, if res_sys does not include a record
    with this id number, it will raise a NotExist Exception, or it will return a new hotel system which has removed
    the record whose id number is equal to the given id number(res_id).    
   *)
   fun cancel res_sys res_id = 
   let
   in
       if (not (lookup_record_helper res_id (#resrecord_list(res_sys : ressys))))
       then
          raise NotExist
       else
          {resrecord_list = (cancel_record_helper pred_equal (#resrecord_list(res_sys)) res_id), total_num_tuple = (#total_num_tuple(res_sys))}
  end
   handle
      NotExist => (print("Can not canel a not-existed record\n");res_sys);
     

   (*
    Give a hotel system(res_sys) and a date(res_date) to the function, it will return the number of complete
    stay by the given date(res_date).
   *)
   fun completedStays res_sys res_date = 
      if (#resrecord_list(res_sys : ressys)) = nil
      then
          0
      else if ((#DateIn(hd(#resrecord_list(res_sys)))) + (#NumN(hd(#resrecord_list(res_sys)))) - 1) < res_date
      then   
         1 + completedStays {resrecord_list=(tl(#resrecord_list(res_sys))),total_num_tuple=(#total_num_tuple(res_sys))} res_date
      else
         completedStays {resrecord_list=(tl(#resrecord_list(res_sys))),total_num_tuple=(#total_num_tuple(res_sys))} res_date;
       
   (*
     Give a record list(res_rec_list) and a date(res_date) to the function, it will return a new record list
     which has removed all the records that have been completed by the given date(res_date). 
   *)
   fun remove_comp_helper res_rec_list res_date = 
       if (res_rec_list = [])
       then
       [] 
       else if ((#DateIn(hd(res_rec_list : resrecord list))) + (#NumN(hd(res_rec_list))) - 1) < res_date
       then    
           remove_comp_helper (tl(res_rec_list)) res_date
       else
           (hd(res_rec_list)) :: (remove_comp_helper (tl(res_rec_list)) res_date);
   
   (*
    Give a hotel system(res_sys) and a date(res_date) to the function, it will return a new hotel system which 
    has removed all records that have been completed by the give date(res_date).
   *)
   fun removeCompletedStays res_sys res_date = 
       {resrecord_list=(remove_comp_helper (#resrecord_list(res_sys : ressys)) res_date),total_num_tuple=(#total_num_tuple(res_sys))};
   
   (*
    Give a hotel system(res_sys) and a date(res_date) to the function, it will return the number of guests
    in the hotel in the given date(res_date). 
   *)
   fun guestQuantity res_sys res_date =  
       if ((#resrecord_list(res_sys : ressys)) = nil)
       then
        0
        else if (lookup_date_helper (#DateIn(hd(#resrecord_list(res_sys)))) (#NumN(hd(#resrecord_list(res_sys)))) res_date)
        then
          (#NumO(hd(#resrecord_list(res_sys)))) + (guestQuantity {resrecord_list = (tl(#resrecord_list(res_sys))),total_num_tuple=(#total_num_tuple(res_sys))} res_date )
        else
           (guestQuantity {resrecord_list = (tl(#resrecord_list(res_sys))),total_num_tuple=(#total_num_tuple(res_sys))} res_date)     
end;

(*test bed*)
(*Define a hotel configure*)
structure HiltonRoomDetail : ROOMDETAIL = 
struct
   val doubleAvailable = 10
   val kingAvailable = 7
   val queenAvailable = 4
   val minnights = SOME 2
   val occupancyLimit = 4
end;

(*Produce a structure which represents a library of HiltonHotelReservations system*)
structure HiltonHotelReservations = MakeHotel(structure Q = HiltonRoomDetail);

(*Instantiate the HiltonHotel Reservation structure*)
val res_sys = HiltonHotelReservations.empty;

(*Produce 20 records*)
(*
This is the reserve map which can help you to check my code.
0 1 2 3 4 5 6 7 8 9 dateline

0 1 2 3             Double(3 occupants)
0 1 2               Double(2)
0 1 2 3 4           Double(2)
  1 2 3 4 5         Double(1)
  1 2 3             Double(2)
  1 2 3 4 5 6       Double(4)
    2 3 4           Double(3)
    2 3 4 5         Double(1)
    2 3 4 5 6       Double(1)

      3 4 5 6 7     King(3)
      3 4 5         King(4)
      3 4 5         King(2)
      3 4 5         King(1)
        4 5 6       King(4)
        4 5         King(3)
          5 6 7     King(2)
 
          5 6 7 8   Queen(1)  
            6 7     Queen(2)
            6 7 8 9 Queen(1)
            6 7 8   Queen(3)
*)                                                                                                                                     
val res_rec1 = HiltonHotelReservations.resrecord_fetch 1 "Mary" "Chen" 0 4 3 HiltonHotelReservations.Double
val res_rec2 = HiltonHotelReservations.resrecord_fetch 2 "Tommy" "Albert" 0 3 2 HiltonHotelReservations.Double
val res_rec3 = HiltonHotelReservations.resrecord_fetch 3 "Richard" "Paul" 0 5 2 HiltonHotelReservations.Double
val res_rec4 = HiltonHotelReservations.resrecord_fetch 4 "Li" "Zhao" 1 5 1 HiltonHotelReservations.Double
val res_rec5 = HiltonHotelReservations.resrecord_fetch 5 "Lewis" "Stephen" 1 3 2 HiltonHotelReservations.Double
val res_rec6 = HiltonHotelReservations.resrecord_fetch 6 "Betty" "Hulda" 1 6 4 HiltonHotelReservations.Double
val res_rec7 = HiltonHotelReservations.resrecord_fetch 7 "Mi" "Yang" 2 3 3 HiltonHotelReservations.Double
val res_rec8 = HiltonHotelReservations.resrecord_fetch 8 "Leona" "Honey" 2 4 1 HiltonHotelReservations.Double
val res_rec9 = HiltonHotelReservations.resrecord_fetch 9 "Sandy" "Wang" 2 5 1 HiltonHotelReservations.Double
val res_rec10 = HiltonHotelReservations.resrecord_fetch 10 "Jeff" "Li" 3 5 3 HiltonHotelReservations.King
val res_rec11 = HiltonHotelReservations.resrecord_fetch 11 "Scott" "Kevin" 3 3 4 HiltonHotelReservations.King
val res_rec12 = HiltonHotelReservations.resrecord_fetch 12 "Vincent" "Taylor" 3 3 2 HiltonHotelReservations.King
val res_rec13 = HiltonHotelReservations.resrecord_fetch 13 "Robert" "Zhao" 3 3 1 HiltonHotelReservations.King
val res_rec14 = HiltonHotelReservations.resrecord_fetch 14 "Andy" "Wang" 4 3 4 HiltonHotelReservations.King
val res_rec15 = HiltonHotelReservations.resrecord_fetch 15 "Lilith" "Melissa" 4 2 3 HiltonHotelReservations.King
val res_rec16 = HiltonHotelReservations.resrecord_fetch 16 "Ming" "Chen" 5 3 2 HiltonHotelReservations.King
val res_rec17 = HiltonHotelReservations.resrecord_fetch 17 "Cheng" "Zhao" 5 4 1 HiltonHotelReservations.Queen
val res_rec18 = HiltonHotelReservations.resrecord_fetch 18 "Francis" "Stephen" 6 2 2 HiltonHotelReservations.Queen
val res_rec19 = HiltonHotelReservations.resrecord_fetch 19 "Penny" "Olive" 6 4 1 HiltonHotelReservations.Queen
val res_rec20 = HiltonHotelReservations.resrecord_fetch 20 "Phyllis" "Chen" 6 3 3 HiltonHotelReservations.Queen;

(*Add these 20 records into the hotel system*)
val res_sys = HiltonHotelReservations.reserve res_sys res_rec1
val res_sys = HiltonHotelReservations.reserve res_sys res_rec2
val res_sys = HiltonHotelReservations.reserve res_sys res_rec3
val res_sys = HiltonHotelReservations.reserve res_sys res_rec4
val res_sys = HiltonHotelReservations.reserve res_sys res_rec5
val res_sys = HiltonHotelReservations.reserve res_sys res_rec6
val res_sys = HiltonHotelReservations.reserve res_sys res_rec7
val res_sys = HiltonHotelReservations.reserve res_sys res_rec8
val res_sys = HiltonHotelReservations.reserve res_sys res_rec9
val res_sys = HiltonHotelReservations.reserve res_sys res_rec10
val res_sys = HiltonHotelReservations.reserve res_sys res_rec11
val res_sys = HiltonHotelReservations.reserve res_sys res_rec12
val res_sys = HiltonHotelReservations.reserve res_sys res_rec13
val res_sys = HiltonHotelReservations.reserve res_sys res_rec14
val res_sys = HiltonHotelReservations.reserve res_sys res_rec15
val res_sys = HiltonHotelReservations.reserve res_sys res_rec16
val res_sys = HiltonHotelReservations.reserve res_sys res_rec17
val res_sys = HiltonHotelReservations.reserve res_sys res_rec18
val res_sys = HiltonHotelReservations.reserve res_sys res_rec19
val res_sys = HiltonHotelReservations.reserve res_sys res_rec20;


(*
Fail to reserve because the inventory is not available.
Because the King room has been run out of at date 
*)

val res_rec21 = HiltonHotelReservations.resrecord_fetch 21 "Hui" "Li" 5 3 3 HiltonHotelReservations.King
val res_sys = HiltonHotelReservations.reserve res_sys res_rec21;

(*Fail to reserve because exceed the occupant limit*)
val res_rec22 = HiltonHotelReservations.resrecord_fetch 22 "Hui" "Li" 5 3 5 HiltonHotelReservations.Double
val res_sys = HiltonHotelReservations.reserve res_sys res_rec22;

(*Fail to reserve because the night is less than the minimal night number*)
val res_rec23 = HiltonHotelReservations.resrecord_fetch 23 "Hui" "Li" 5 1 2 HiltonHotelReservations.Double
val res_sys = HiltonHotelReservations.reserve res_sys res_rec23;

(*cancel three reservations:first is id=18, second is id=19, third is id=20*)
val res_sys = HiltonHotelReservations.cancel res_sys 18;
val res_sys = HiltonHotelReservations.cancel res_sys 19;
val res_sys = HiltonHotelReservations.cancel res_sys 20;

(*
Query the inventory for Double room at date 4.
At date 4, there are 6 Double rooms reserved by guests, so the inventory is 4.
*)
val room_inventory = HiltonHotelReservations.getInventory res_sys HiltonHotelReservations.Double 4;

(*
Get the completed stays by date 5.
There are 5 completed stays:stay with id = 1,with id = 2,with id = 3,with id = 5, with id = 7.
*)
val stay_completed = HiltonHotelReservations.completedStays res_sys 5;
(*Remove the completed stays by date 5*)
val res_sys = HiltonHotelReservations.removeCompletedStays res_sys 5;
(*
Get the number of completed stays by date 5.
Because these stays have been removed above, so the number is 0.
*)
val stay_completed = HiltonHotelReservations.completedStays res_sys 5;

(*
Get the number of guests at date 5.
The number of occupant is 27(see the reserve map)
*)
val total_guests = HiltonHotelReservations.guestQuantity res_sys 5;
