;
; Types.
; 

%Value = type { i64, i64 }
%Pair = type { i64, %Value, %Value }

;
; External methods.
; 

declare ptr @malloc(i64)
declare void @free(ptr)

;
; Constructors.
; 

define %Value @new_value(i64 %type, i64 %value) alwaysinline {
  %result.0 = insertvalue %Value poison, i64 %type, 0
  %result.1 = insertvalue %Value %result.0, i64 %value, 1
  ret %Value %result.1
}

define %Value @new_nil() alwaysinline {
  ret %Value { i64 0, i64 0 }
}
  
define %Value @new_tru() alwaysinline {
  ret %Value { i64 1, i64 0 }
}

define %Value @new_chr(i64 %value) alwaysinline {
  %result = insertvalue %Value { i64 2, i64 poison }, i64 %value, 1
  ret %Value %result
}

define %Value @new_int(i64 %value) alwaysinline {
  %result = insertvalue %Value { i64 3, i64 poison }, i64 %value, 1
  ret %Value %result
}

define %Value @new_ext(i64 %value) alwaysinline {
  %result = insertvalue %Value { i64 4, i64 poison }, i64 %value, 1
  ret %Value %result
}

define %Value @new_fun(i64 %value) alwaysinline {
  %result = insertvalue %Value { i64 5, i64 poison }, i64 %value, 1
  ret %Value %result
}

define %Value @new_wld() alwaysinline {
  ret %Value { i64 6, i64 0 }
}

;
; Clone.
;

define internal %Value @clone_noop(%Value %val) alwaysinline {
  ret %Value %val
}

define internal %Value @clone_pair(%Value %val) {
  %pptr = extractvalue %Value %val, 1
  %pair = inttoptr i64 %pptr to %Pair*
  %refc = getelementptr %Pair, %Pair* %pair, i32 0, i32 0
  %cntr = load i64, i64* %refc
  %next = add i64 %cntr, 1
  store i64 %next, i64* %refc
  ret %Value %val
}

@CLONE = internal global [16 x %Value(%Value)*] [ 
  %Value(%Value)* @clone_noop, ; Nil
  %Value(%Value)* @clone_noop, ; True
  %Value(%Value)* @clone_noop, ; Char
  %Value(%Value)* @clone_noop, ; Number
  %Value(%Value)* @clone_noop, ; Extcall
  %Value(%Value)* @clone_noop, ; Funcall
  %Value(%Value)* @clone_noop, ; Wildcard
  %Value(%Value)* null,        ; 
  %Value(%Value)* null,        ; 
  %Value(%Value)* null,        ; 
  %Value(%Value)* null,        ; Bytes 
  %Value(%Value)* null,        ; Closure
  %Value(%Value)* null,        ; Link
  %Value(%Value)* @clone_pair, ; Pair
  %Value(%Value)* null,        ; String 
  %Value(%Value)* null         ; Symbol
]

define %Value @clone(%Value %val) alwaysinline {
  %indx = extractvalue %Value %val, 0
  %fptr = getelementptr [16 x %Value(%Value)*], [16 x %Value(%Value)*]* @CLONE, i32 0, i64 %indx
  %func = load %Value(%Value)*, %Value(%Value)** %fptr
  %rslt = call %Value %func(%Value %val)
  ret %Value %rslt
}

;
; Drop.
; 

define internal void @drop_noop(%Value %val) alwaysinline {
  ret void
}

define internal void @drop_pair(%Value %val) {
  %pptr = extractvalue %Value %val, 1
  %pair = inttoptr i64 %pptr to %Pair*
  %refc = getelementptr %Pair, %Pair* %pair, i32 0, i32 0
  %cntr = load i64, i64* %refc
  %next = sub i64 %cntr, 1
  %cmp1 = icmp eq i64 %next, 0
  br i1 %cmp1, label %drop, label %store
  ;
  ; Drop the pair if the reference counter is 0.
  ; 
drop:
  %pcar = getelementptr %Pair, %Pair* %pair, i32 0, i32 1
  %vcar = load %Value, %Value* %pcar
  call void @drop(%Value %vcar)
  %pcdr = getelementptr %Pair, %Pair* %pair, i32 0, i32 2
  %vcdr = load %Value, %Value* %pcdr
  call void @drop(%Value %vcdr)
  call void @free(ptr %pair)
  br label %done
  ;
  ; Otherwise, store the decremented reference counter.
  ; 
store:
  store i64 %next, i64* %refc
  br label %done
  ;
  ; Done.
  ; 
done:
  ret void
}

@DROP = internal global [16 x void(%Value)*] [ 
  %Value(%Value)* @drop_noop, ; Nil
  %Value(%Value)* @drop_noop, ; True
  %Value(%Value)* @drop_noop, ; Char
  %Value(%Value)* @drop_noop, ; Number
  %Value(%Value)* @drop_noop, ; Extcall
  %Value(%Value)* @drop_noop, ; Funcall
  %Value(%Value)* @drop_noop, ; Wildcard
  %Value(%Value)* null,       ; 
  %Value(%Value)* null,       ; 
  %Value(%Value)* null,       ; 
  %Value(%Value)* null,       ; Bytes 
  %Value(%Value)* null,       ; Closure
  %Value(%Value)* null,       ; Link
  %Value(%Value)* @drop_pair, ; Pair
  %Value(%Value)* null,       ; String 
  %Value(%Value)* null        ; Symbol
]

define void @drop(%Value %val) alwaysinline {
  %indx = extractvalue %Value %val, 0
  %fptr = getelementptr [16 x %Value(%Value)*], [16 x %Value(%Value)*]* @DROP, i32 0, i64 %indx
  %func = load %Value(%Value)*, %Value(%Value)** %fptr
  call %Value %func(%Value %val)
  ret void
}

;
; Arithmetic operations.
;

define %Value @add(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = add i64 %v0, %v1
  %rv = call %Value @new_int(i64 %rs)
  ret %Value %rv
}

define %Value @sub(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = sub i64 %v0, %v1
  %rv = call %Value @new_int(i64 %rs)
  ret %Value %rv
}

define %Value @mul(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = mul i64 %v0, %v1
  %rv = call %Value @new_int(i64 %rs)
  ret %Value %rv
}

define %Value @div(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = sdiv i64 %v0, %v1
  %rv = call %Value @new_int(i64 %rs)
  ret %Value %rv
}

define %Value @mod(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = srem i64 %v0, %v1
  %rv = call %Value @new_int(i64 %rs)
  ret %Value %rv
}

;
; Comparison operations.
;

define %Value @ge(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rb = icmp sge i64 %v0, %v1
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

define %Value @gt(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rb = icmp sgt i64 %v0, %v1
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

define %Value @le(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rb = icmp sle i64 %v0, %v1
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

define %Value @lt(%Value %a, %Value %b) alwaysinline {
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rb = icmp slt i64 %v0, %v1
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

;
; Bitwise operations.
;

define %Value @bitand(%Value %a, %Value %b) alwaysinline {
  %vt = extractvalue %Value %a, 0
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = and i64 %v0, %v1
  %rv = call %Value @new_value(i64 %vt, i64 %rs)
  ret %Value %rv
}

define %Value @bitor(%Value %a, %Value %b) alwaysinline {
  %vt = extractvalue %Value %a, 0
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = or i64 %v0, %v1
  %rv = call %Value @new_value(i64 %vt, i64 %rs)
  ret %Value %rv
}

define %Value @bitxor(%Value %a, %Value %b) alwaysinline {
  %vt = extractvalue %Value %a, 0
  %v0 = extractvalue %Value %a, 1
  %v1 = extractvalue %Value %b, 1
  %rs = xor i64 %v0, %v1
  %rv = call %Value @new_value(i64 %vt, i64 %rs)
  ret %Value %rv
}

;
; Boolean operations.
; 

define %Value @and(%Value %a, %Value %b) alwaysinline {
  %ro = call %Value @bitand(%Value %a, %Value %b)
  %rr = extractvalue %Value %ro, 1
  %rb = icmp ne i64 %rr, 0
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

define %Value @or(%Value %a, %Value %b) alwaysinline {
  %ro = call %Value @bitor(%Value %a, %Value %b)
  %rr = extractvalue %Value %ro, 1
  %rb = icmp ne i64 %rr, 0
  %rs = zext i1 %rb to i64 
  %rv = call %Value @new_value(i64 %rs, i64 0)
  ret %Value %rv
}

;
; List operations.
; 

define %Value @car(%Value %val) {
  ;
  ; Check the value type.
  ; 
  %type = extractvalue %Value %val, 0
  %comp = icmp eq i64 %type, 13
  br i1 %comp, label %then, label %else
  ;
  ; Handle pairs.
  ; 
then:
  %pptr = extractvalue %Value %val, 1
  %pair = inttoptr i64 %pptr to %Pair*
  %pcar = getelementptr %Pair, %Pair* %pair, i32 0, i32 1
  %vcar = load %Value, %Value* %pcar
  %rcar = call %Value @clone(%Value %vcar) 
  br label %done
  ;
  ; Handle other types.
  ; 
else:
  %rnil = call %Value @new_nil()
  br label %done
  ;
  ; Done.
  ; 
done:
  %rslt = phi %Value [ %rcar, %then ], [ %rnil, %else ]
  call void @drop(%Value %val)
  ret %Value %rslt
}

define %Value @cdr(%Value %val) {
  ;
  ; Check the value type.
  ; 
  %type = extractvalue %Value %val, 0
  %comp = icmp eq i64 %type, 13
  br i1 %comp, label %then, label %else
  ;
  ; Handle pairs.
  ; 
then:
  %pptr = extractvalue %Value %val, 1
  %pair = inttoptr i64 %pptr to %Pair*
  %pcar = getelementptr %Pair, %Pair* %pair, i32 0, i32 2
  %vcar = load %Value, %Value* %pcar
  %rcar = call %Value @clone(%Value %vcar) 
  br label %done
  ;
  ; Handle other types.
  ; 
else:
  %rnil = call %Value @new_nil()
  br label %done
  ;
  ; Done.
  ; 
done:
  %rslt = phi %Value [ %rcar, %then ], [ %rnil, %else ]
  call void @drop(%Value %val)
  ret %Value %rslt
}

define %Value @cons(%Value %car, %Value %cdr) {
  ; 
  ; Allocate the pair through malloc (40 bytes).
  ; 
  %pair = call ptr @malloc(i64 40)
  ; 
  ; Initialize the reference counter.
  ; 
  %refc = getelementptr %Pair, %Pair* %pair, i32 0, i32 0
  store i64 1, i64* %refc 
  ;
  ; Initialize the CAR member.
  ; 
  %rcar = getelementptr %Pair, %Pair* %pair, i32 0, i32 1
  store %Value %car, %Value* %rcar
  ;
  ; Initialize the CDR member.
  ; 
  %rcdr = getelementptr %Pair, %Pair* %pair, i32 0, i32 2
  store %Value %cdr, %Value* %rcdr
  ;
  ; Initialize the value.
  ;
  %pptr = ptrtoint %Pair* %pair to i64
  %rslt = insertvalue %Value { i64 13, i64 poison }, i64 %pptr, 1  
  ;
  ; Done.
  ; 
  ret %Value %rslt
}
