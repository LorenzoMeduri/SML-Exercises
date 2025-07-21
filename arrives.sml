(* 
Given the datatype:
    datatype code = red of string
    | yellow of string
    | green of string;
which represents a patient arriving at the emergency room.
The string represents the patient's surname, while the three different constructors red, yellow and green represent the patient's severity
(red code: maximum urgency, green code: minimum urgency).
When a patient with red code arrives at the emergency room, they are put on the waiting list after all patients with red code (but before
those with yellow or green code); 
when a patient with yellow code arrives, they are put on the waiting list after all patients with red or yellow code (but before those with green code), 
while when a patient with green code arrives they are put on the waiting list after all other patients.
Write a function arrives (having type code list -> code -> code list) that receives as arguments the list of waiting patients
and a newly arrived patient (element of type code) and returns the updated list of waiting patients (after having
inserted the new patient in the correct place of the queue).
*)

datatype code = red of string
    | yellow of string
    | green of string;

fun arrives [] = (fn (e:code) => [e]) | arrives ((e::l):code list) = 
    fn (red p) => 
        (case e of
            red _ => e::(arrives l (red p)) |
            _ => ((red p)::e::l))
    |  (yellow p) => 
        (case e of
            green _ => (yellow p)::e::l |
            _ => e::(arrives l (yellow p)))
    |  (green p) => (e::l)@[(green p)];