; ModuleID = 'test_box.91eb2609bff77acc-cgu.0'
source_filename = "test_box.91eb2609bff77acc-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@vtable.0 = private unnamed_addr constant <{ [24 x i8], ptr, ptr, ptr }> <{ [24 x i8] c"\00\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00", ptr @"_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h55c4099b98402febE", ptr @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h66ba14764814f21cE", ptr @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h66ba14764814f21cE" }>, align 8
@alloc_560a59ed819b9d9a5841f6e731c4c8e5 = private unnamed_addr constant [210 x i8] c"unsafe precondition(s) violated: NonNull::new_unchecked requires that the pointer is non-null\0A\0AThis indicates a bug in the program. This Undefined Behavior check is optional, and cannot be relied on for safety.", align 1
@anon.295b57f263710f4f7024ece6804cccce.0 = private unnamed_addr constant <{ [8 x i8], [8 x i8] }> <{ [8 x i8] zeroinitializer, [8 x i8] undef }>, align 8
@alloc_1be5ea12ba708d9a11b6e93a7d387a75 = private unnamed_addr constant [281 x i8] c"unsafe precondition(s) violated: Layout::from_size_align_unchecked requires that align is a power of 2 and the rounded-up allocation size does not exceed isize::MAX\0A\0AThis indicates a bug in the program. This Undefined Behavior check is optional, and cannot be relied on for safety.", align 1
@alloc_767e663c14edec40132e585b351fbcb5 = private unnamed_addr constant [124 x i8] c"/home/runner/.rustup/toolchains/nightly-2025-07-08-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/alloc/src/alloc.rs\00", align 1
@alloc_aad4da8df43a82bed20ae0f57be6cfa9 = private unnamed_addr constant <{ ptr, [16 x i8] }> <{ ptr @alloc_767e663c14edec40132e585b351fbcb5, [16 x i8] c"{\00\00\00\00\00\00\00^\01\00\00\1B\00\00\00" }>, align 8
@alloc_398e481c8bfe9b9c08e9688f16354916 = private unnamed_addr constant [130 x i8] c"/home/runner/.rustup/toolchains/nightly-2025-07-08-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ptr/non_null.rs\00", align 1
@alloc_bb7aa2b51a865daaef7f3fb08324f7dd = private unnamed_addr constant <{ ptr, [16 x i8] }> <{ ptr @alloc_398e481c8bfe9b9c08e9688f16354916, [16 x i8] c"\81\00\00\00\00\00\00\00l\05\00\00\12\00\00\00" }>, align 8
@alloc_ff9e560c98f7f72a40ac8e61348b6c7e = private unnamed_addr constant <{ ptr, [16 x i8] }> <{ ptr @alloc_398e481c8bfe9b9c08e9688f16354916, [16 x i8] c"\81\00\00\00\00\00\00\00\09\01\00\00\1B\00\00\00" }>, align 8
@alloc_2c5ae72b005079a5f9ea3a7dd4ffcd5b = private unnamed_addr constant [130 x i8] c"/home/runner/.rustup/toolchains/nightly-2025-07-08-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/alloc/layout.rs\00", align 1
@alloc_dce6ac8e6908cf2ed7e1e00c561d68e7 = private unnamed_addr constant <{ ptr, [16 x i8] }> <{ ptr @alloc_2c5ae72b005079a5f9ea3a7dd4ffcd5b, [16 x i8] c"\81\00\00\00\00\00\00\00\E0\00\00\00\12\00\00\00" }>, align 8

; std::rt::lang_start
; Function Attrs: nonlazybind uwtable
define hidden i64 @_ZN3std2rt10lang_start17hb1e4731a654e3d4cE(ptr %main, i64 %argc, ptr %argv, i8 %sigpipe) unnamed_addr #0 {
start:
  %_7 = alloca [8 x i8], align 8
  store ptr %main, ptr %_7, align 8
; call std::rt::lang_start_internal
  %_0 = call i64 @_ZN3std2rt19lang_start_internal17hca2222f6af67d268E(ptr align 1 %_7, ptr align 8 @vtable.0, i64 %argc, ptr %argv, i8 %sigpipe)
  ret i64 %_0
}

; std::rt::lang_start::{{closure}}
; Function Attrs: inlinehint nonlazybind uwtable
define internal i32 @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h66ba14764814f21cE"(ptr align 8 %_1) unnamed_addr #1 {
start:
  %_4 = load ptr, ptr %_1, align 8
; call std::sys::backtrace::__rust_begin_short_backtrace
  call void @_ZN3std3sys9backtrace28__rust_begin_short_backtrace17h5307b1d1b3333755E(ptr %_4)
; call <() as std::process::Termination>::report
  %self = call i8 @"_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h80308814c80236c4E"()
  %_0 = zext i8 %self to i32
  ret i32 %_0
}

; std::sys::backtrace::__rust_begin_short_backtrace
; Function Attrs: noinline nonlazybind uwtable
define internal void @_ZN3std3sys9backtrace28__rust_begin_short_backtrace17h5307b1d1b3333755E(ptr %f) unnamed_addr #2 {
start:
; call core::ops::function::FnOnce::call_once
  call void @_ZN4core3ops8function6FnOnce9call_once17hda2ff85b774c68e1E(ptr %f)
  call void asm sideeffect "", "~{memory}"(), !srcloc !4
  ret void
}

; core::mem::drop
; Function Attrs: inlinehint nonlazybind uwtable
define internal void @_ZN4core3mem4drop17h9064afdcf4adc51bE(ptr align 4 %0) unnamed_addr #1 {
start:
  %_x = alloca [8 x i8], align 8
  store ptr %0, ptr %_x, align 8
; call core::ptr::drop_in_place<alloc::boxed::Box<i32>>
  call void @"_ZN4core3ptr49drop_in_place$LT$alloc..boxed..Box$LT$i32$GT$$GT$17h2aae03ee63399816E"(ptr align 8 %_x)
  ret void
}

; core::ops::function::FnOnce::call_once{{vtable.shim}}
; Function Attrs: inlinehint nonlazybind uwtable
define internal i32 @"_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h55c4099b98402febE"(ptr %_1) unnamed_addr #1 {
start:
  %_2 = alloca [0 x i8], align 1
  %0 = load ptr, ptr %_1, align 8
; call core::ops::function::FnOnce::call_once
  %_0 = call i32 @_ZN4core3ops8function6FnOnce9call_once17hc342886af894dfb0E(ptr %0)
  ret i32 %_0
}

; core::ops::function::FnOnce::call_once
; Function Attrs: inlinehint nonlazybind uwtable
define internal i32 @_ZN4core3ops8function6FnOnce9call_once17hc342886af894dfb0E(ptr %0) unnamed_addr #1 personality ptr @rust_eh_personality {
start:
  %1 = alloca [16 x i8], align 8
  %_2 = alloca [0 x i8], align 1
  %_1 = alloca [8 x i8], align 8
  store ptr %0, ptr %_1, align 8
; invoke std::rt::lang_start::{{closure}}
  %_0 = invoke i32 @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h66ba14764814f21cE"(ptr align 8 %_1)
          to label %bb1 unwind label %cleanup

bb3:                                              ; preds = %cleanup
  %2 = load ptr, ptr %1, align 8
  %3 = getelementptr inbounds i8, ptr %1, i64 8
  %4 = load i32, ptr %3, align 8
  %5 = insertvalue { ptr, i32 } poison, ptr %2, 0
  %6 = insertvalue { ptr, i32 } %5, i32 %4, 1
  resume { ptr, i32 } %6

cleanup:                                          ; preds = %start
  %7 = landingpad { ptr, i32 }
          cleanup
  %8 = extractvalue { ptr, i32 } %7, 0
  %9 = extractvalue { ptr, i32 } %7, 1
  store ptr %8, ptr %1, align 8
  %10 = getelementptr inbounds i8, ptr %1, i64 8
  store i32 %9, ptr %10, align 8
  br label %bb3

bb1:                                              ; preds = %start
  ret i32 %_0
}

; core::ops::function::FnOnce::call_once
; Function Attrs: inlinehint nonlazybind uwtable
define internal void @_ZN4core3ops8function6FnOnce9call_once17hda2ff85b774c68e1E(ptr %_1) unnamed_addr #1 {
start:
  %_2 = alloca [0 x i8], align 1
  call void %_1()
  ret void
}

; core::ptr::drop_in_place<alloc::boxed::Box<i32>>
; Function Attrs: nonlazybind uwtable
define internal void @"_ZN4core3ptr49drop_in_place$LT$alloc..boxed..Box$LT$i32$GT$$GT$17h2aae03ee63399816E"(ptr align 8 %_1) unnamed_addr #0 personality ptr @rust_eh_personality {
start:
  %0 = alloca [16 x i8], align 8
  %_6 = load ptr, ptr %_1, align 8
  br label %bb3

bb3:                                              ; preds = %start
; call <alloc::boxed::Box<T,A> as core::ops::drop::Drop>::drop
  call void @"_ZN72_$LT$alloc..boxed..Box$LT$T$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hba8de0dbeb02cdf5E"(ptr align 8 %_1)
  ret void

bb4:                                              ; No predecessors!
; invoke <alloc::boxed::Box<T,A> as core::ops::drop::Drop>::drop
  invoke void @"_ZN72_$LT$alloc..boxed..Box$LT$T$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hba8de0dbeb02cdf5E"(ptr align 8 %_1) #12
          to label %bb1 unwind label %terminate

terminate:                                        ; preds = %bb4
  %1 = landingpad { ptr, i32 }
          filter [0 x ptr] zeroinitializer
; call core::panicking::panic_in_cleanup
  call void @_ZN4core9panicking16panic_in_cleanup17h26d5998e8f171caaE() #13
  unreachable

bb1:                                              ; preds = %bb4
  %2 = load ptr, ptr %0, align 8
  %3 = getelementptr inbounds i8, ptr %0, i64 8
  %4 = load i32, ptr %3, align 8
  %5 = insertvalue { ptr, i32 } poison, ptr %2, 0
  %6 = insertvalue { ptr, i32 } %5, i32 %4, 1
  resume { ptr, i32 } %6
}

; core::ptr::non_null::NonNull<T>::new_unchecked::precondition_check
; Function Attrs: inlinehint nounwind nonlazybind uwtable
define internal void @"_ZN4core3ptr8non_null16NonNull$LT$T$GT$13new_unchecked18precondition_check17hd6d313f6313968c0E"(ptr %ptr, ptr align 8 %0) unnamed_addr #3 {
start:
  %_5 = alloca [16 x i8], align 8
  %_3 = alloca [48 x i8], align 8
  %_6 = ptrtoint ptr %ptr to i64
  %1 = icmp eq i64 %_6, 0
  br i1 %1, label %bb1, label %bb2

bb1:                                              ; preds = %start
  %2 = getelementptr inbounds nuw { ptr, i64 }, ptr %_5, i64 0
  store ptr @alloc_560a59ed819b9d9a5841f6e731c4c8e5, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %2, i64 8
  store i64 210, ptr %3, align 8
  store ptr %_5, ptr %_3, align 8
  %4 = getelementptr inbounds i8, ptr %_3, i64 8
  store i64 1, ptr %4, align 8
  %5 = load ptr, ptr @anon.295b57f263710f4f7024ece6804cccce.0, align 8
  %6 = load i64, ptr getelementptr inbounds (i8, ptr @anon.295b57f263710f4f7024ece6804cccce.0, i64 8), align 8
  %7 = getelementptr inbounds i8, ptr %_3, i64 32
  store ptr %5, ptr %7, align 8
  %8 = getelementptr inbounds i8, ptr %7, i64 8
  store i64 %6, ptr %8, align 8
  %9 = getelementptr inbounds i8, ptr %_3, i64 16
  store ptr inttoptr (i64 8 to ptr), ptr %9, align 8
  %10 = getelementptr inbounds i8, ptr %9, i64 8
  store i64 0, ptr %10, align 8
; call core::panicking::panic_nounwind_fmt
  call void @_ZN4core9panicking18panic_nounwind_fmt17h991b22c5832142abE(ptr align 8 %_3, i1 zeroext false, ptr align 8 %0) #14
  unreachable

bb2:                                              ; preds = %start
  ret void
}

; core::alloc::layout::Layout::from_size_align_unchecked::precondition_check
; Function Attrs: inlinehint nounwind nonlazybind uwtable
define internal void @_ZN4core5alloc6layout6Layout25from_size_align_unchecked18precondition_check17h8c791635264a3c10E(i64 %size, i64 %align, ptr align 8 %0) unnamed_addr #3 personality ptr @rust_eh_personality {
start:
  %_7 = alloca [16 x i8], align 8
  %_5 = alloca [48 x i8], align 8
; invoke core::alloc::layout::Layout::is_size_align_valid
  %_3 = invoke zeroext i1 @_ZN4core5alloc6layout6Layout19is_size_align_valid17haacdf0c659f9060fE(i64 %size, i64 %align)
          to label %bb1 unwind label %terminate

terminate:                                        ; preds = %start
  %1 = landingpad { ptr, i32 }
          filter [0 x ptr] zeroinitializer
; call core::panicking::panic_cannot_unwind
  call void @_ZN4core9panicking19panic_cannot_unwind17h67b01b7815c95828E() #13
  unreachable

bb1:                                              ; preds = %start
  br i1 %_3, label %bb2, label %bb3

bb3:                                              ; preds = %bb1
  %2 = getelementptr inbounds nuw { ptr, i64 }, ptr %_7, i64 0
  store ptr @alloc_1be5ea12ba708d9a11b6e93a7d387a75, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %2, i64 8
  store i64 281, ptr %3, align 8
  store ptr %_7, ptr %_5, align 8
  %4 = getelementptr inbounds i8, ptr %_5, i64 8
  store i64 1, ptr %4, align 8
  %5 = load ptr, ptr @anon.295b57f263710f4f7024ece6804cccce.0, align 8
  %6 = load i64, ptr getelementptr inbounds (i8, ptr @anon.295b57f263710f4f7024ece6804cccce.0, i64 8), align 8
  %7 = getelementptr inbounds i8, ptr %_5, i64 32
  store ptr %5, ptr %7, align 8
  %8 = getelementptr inbounds i8, ptr %7, i64 8
  store i64 %6, ptr %8, align 8
  %9 = getelementptr inbounds i8, ptr %_5, i64 16
  store ptr inttoptr (i64 8 to ptr), ptr %9, align 8
  %10 = getelementptr inbounds i8, ptr %9, i64 8
  store i64 0, ptr %10, align 8
; call core::panicking::panic_nounwind_fmt
  call void @_ZN4core9panicking18panic_nounwind_fmt17h991b22c5832142abE(ptr align 8 %_5, i1 zeroext false, ptr align 8 %0) #14
  unreachable

bb2:                                              ; preds = %bb1
  ret void
}

; <() as std::process::Termination>::report
; Function Attrs: inlinehint nonlazybind uwtable
define internal i8 @"_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17h80308814c80236c4E"() unnamed_addr #1 {
start:
  ret i8 0
}

; alloc::alloc::exchange_malloc
; Function Attrs: inlinehint nonlazybind uwtable
define internal ptr @_ZN5alloc5alloc15exchange_malloc17h74c37b16390a9bebE(i64 %size, i64 %align) unnamed_addr #1 {
start:
  %_4 = alloca [16 x i8], align 8
  br label %bb4

bb4:                                              ; preds = %start
; call core::alloc::layout::Layout::from_size_align_unchecked::precondition_check
  call void @_ZN4core5alloc6layout6Layout25from_size_align_unchecked18precondition_check17h8c791635264a3c10E(i64 %size, i64 %align, ptr align 8 @alloc_aad4da8df43a82bed20ae0f57be6cfa9) #15
  br label %bb5

bb5:                                              ; preds = %bb4
; call alloc::alloc::Global::alloc_impl
  %0 = call { ptr, i64 } @_ZN5alloc5alloc6Global10alloc_impl17he51137ab3bb5e94fE(ptr align 1 inttoptr (i64 1 to ptr), i64 %align, i64 %size, i1 zeroext false)
  %1 = extractvalue { ptr, i64 } %0, 0
  %2 = extractvalue { ptr, i64 } %0, 1
  store ptr %1, ptr %_4, align 8
  %3 = getelementptr inbounds i8, ptr %_4, i64 8
  store i64 %2, ptr %3, align 8
  %4 = load ptr, ptr %_4, align 8
  %5 = getelementptr inbounds i8, ptr %_4, i64 8
  %6 = load i64, ptr %5, align 8
  %7 = ptrtoint ptr %4 to i64
  %8 = icmp eq i64 %7, 0
  %_5 = select i1 %8, i64 1, i64 0
  %9 = trunc nuw i64 %_5 to i1
  br i1 %9, label %bb2, label %bb3

bb2:                                              ; preds = %bb5
; call alloc::alloc::handle_alloc_error
  call void @_ZN5alloc5alloc18handle_alloc_error17h503507c06789fd82E(i64 %align, i64 %size) #16
  unreachable

bb3:                                              ; preds = %bb5
  %ptr.0 = load ptr, ptr %_4, align 8
  %10 = getelementptr inbounds i8, ptr %_4, i64 8
  %ptr.1 = load i64, ptr %10, align 8
  ret ptr %ptr.0

bb1:                                              ; No predecessors!
  unreachable
}

; alloc::alloc::Global::alloc_impl
; Function Attrs: inlinehint nonlazybind uwtable
define internal { ptr, i64 } @_ZN5alloc5alloc6Global10alloc_impl17he51137ab3bb5e94fE(ptr align 1 %self, i64 %0, i64 %1, i1 zeroext %zeroed) unnamed_addr #1 {
start:
  %self4 = alloca [8 x i8], align 8
  %self3 = alloca [8 x i8], align 8
  %_12 = alloca [8 x i8], align 8
  %layout2 = alloca [16 x i8], align 8
  %layout1 = alloca [16 x i8], align 8
  %raw_ptr = alloca [8 x i8], align 8
  %_0 = alloca [16 x i8], align 8
  %layout = alloca [16 x i8], align 8
  store i64 %0, ptr %layout, align 8
  %2 = getelementptr inbounds i8, ptr %layout, i64 8
  store i64 %1, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %layout, i64 8
  %size = load i64, ptr %3, align 8
  %4 = icmp eq i64 %size, 0
  br i1 %4, label %bb2, label %bb1

bb2:                                              ; preds = %start
  %_19 = load i64, ptr %layout, align 8
  %_20 = getelementptr i8, ptr null, i64 %_19
  %data = getelementptr i8, ptr null, i64 %_19
  br label %bb7

bb1:                                              ; preds = %start
  br i1 %zeroed, label %bb3, label %bb4

bb7:                                              ; preds = %bb2
  %_24 = getelementptr i8, ptr null, i64 %_19
; call core::ptr::non_null::NonNull<T>::new_unchecked::precondition_check
  call void @"_ZN4core3ptr8non_null16NonNull$LT$T$GT$13new_unchecked18precondition_check17hd6d313f6313968c0E"(ptr %_24, ptr align 8 @alloc_bb7aa2b51a865daaef7f3fb08324f7dd) #15
  br label %bb9

bb9:                                              ; preds = %bb7
  store ptr %data, ptr %_0, align 8
  %5 = getelementptr inbounds i8, ptr %_0, i64 8
  store i64 0, ptr %5, align 8
  br label %bb6

bb6:                                              ; preds = %bb21, %bb14, %bb9
  %6 = load ptr, ptr %_0, align 8
  %7 = getelementptr inbounds i8, ptr %_0, i64 8
  %8 = load i64, ptr %7, align 8
  %9 = insertvalue { ptr, i64 } poison, ptr %6, 0
  %10 = insertvalue { ptr, i64 } %9, i64 %8, 1
  ret { ptr, i64 } %10

bb4:                                              ; preds = %bb1
  %11 = load i64, ptr %layout, align 8
  %12 = getelementptr inbounds i8, ptr %layout, i64 8
  %13 = load i64, ptr %12, align 8
  store i64 %11, ptr %layout2, align 8
  %14 = getelementptr inbounds i8, ptr %layout2, i64 8
  store i64 %13, ptr %14, align 8
; call __rustc::__rust_no_alloc_shim_is_unstable_v2
  call void @_RNvCsaNFudIaDR8x_7___rustc35___rust_no_alloc_shim_is_unstable_v2() #15
  %_41 = load i64, ptr %layout, align 8
  %_44 = icmp uge i64 %_41, 1
  %_45 = icmp ule i64 %_41, -9223372036854775808
  %_46 = and i1 %_44, %_45
; call __rustc::__rust_alloc
  %15 = call ptr @_RNvCsaNFudIaDR8x_7___rustc12___rust_alloc(i64 %size, i64 %_41) #15
  store ptr %15, ptr %raw_ptr, align 8
  br label %bb5

bb3:                                              ; preds = %bb1
  %16 = load i64, ptr %layout, align 8
  %17 = getelementptr inbounds i8, ptr %layout, i64 8
  %18 = load i64, ptr %17, align 8
  store i64 %16, ptr %layout1, align 8
  %19 = getelementptr inbounds i8, ptr %layout1, i64 8
  store i64 %18, ptr %19, align 8
; call __rustc::__rust_no_alloc_shim_is_unstable_v2
  call void @_RNvCsaNFudIaDR8x_7___rustc35___rust_no_alloc_shim_is_unstable_v2() #15
  %_31 = load i64, ptr %layout, align 8
  %_34 = icmp uge i64 %_31, 1
  %_35 = icmp ule i64 %_31, -9223372036854775808
  %_36 = and i1 %_34, %_35
; call __rustc::__rust_alloc_zeroed
  %20 = call ptr @_RNvCsaNFudIaDR8x_7___rustc19___rust_alloc_zeroed(i64 %size, i64 %_31) #15
  store ptr %20, ptr %raw_ptr, align 8
  br label %bb5

bb5:                                              ; preds = %bb3, %bb4
  %ptr = load ptr, ptr %raw_ptr, align 8
  %_49 = ptrtoint ptr %ptr to i64
  %21 = icmp eq i64 %_49, 0
  br i1 %21, label %bb14, label %bb15

bb14:                                             ; preds = %bb5
  store ptr null, ptr %self4, align 8
  store ptr null, ptr %self3, align 8
  %22 = load ptr, ptr @anon.295b57f263710f4f7024ece6804cccce.0, align 8
  %23 = load i64, ptr getelementptr inbounds (i8, ptr @anon.295b57f263710f4f7024ece6804cccce.0, i64 8), align 8
  store ptr %22, ptr %_0, align 8
  %24 = getelementptr inbounds i8, ptr %_0, i64 8
  store i64 %23, ptr %24, align 8
  br label %bb6

bb15:                                             ; preds = %bb5
  br label %bb16

bb16:                                             ; preds = %bb15
; call core::ptr::non_null::NonNull<T>::new_unchecked::precondition_check
  call void @"_ZN4core3ptr8non_null16NonNull$LT$T$GT$13new_unchecked18precondition_check17hd6d313f6313968c0E"(ptr %ptr, ptr align 8 @alloc_ff9e560c98f7f72a40ac8e61348b6c7e) #15
  br label %bb18

bb18:                                             ; preds = %bb16
  store ptr %ptr, ptr %self4, align 8
  %v = load ptr, ptr %self4, align 8
  store ptr %v, ptr %self3, align 8
  %v5 = load ptr, ptr %self3, align 8
  store ptr %v5, ptr %_12, align 8
  %ptr6 = load ptr, ptr %_12, align 8
  br label %bb19

bb19:                                             ; preds = %bb18
; call core::ptr::non_null::NonNull<T>::new_unchecked::precondition_check
  call void @"_ZN4core3ptr8non_null16NonNull$LT$T$GT$13new_unchecked18precondition_check17hd6d313f6313968c0E"(ptr %ptr6, ptr align 8 @alloc_bb7aa2b51a865daaef7f3fb08324f7dd) #15
  br label %bb21

bb21:                                             ; preds = %bb19
  store ptr %ptr6, ptr %_0, align 8
  %25 = getelementptr inbounds i8, ptr %_0, i64 8
  store i64 %size, ptr %25, align 8
  br label %bb6
}

; <alloc::alloc::Global as core::alloc::Allocator>::deallocate
; Function Attrs: inlinehint nonlazybind uwtable
define internal void @"_ZN63_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Allocator$GT$10deallocate17h9ca9ff17ef3f71aaE"(ptr align 1 %self, ptr %ptr, i64 %0, i64 %1) unnamed_addr #1 {
start:
  %layout1 = alloca [16 x i8], align 8
  %layout = alloca [16 x i8], align 8
  store i64 %0, ptr %layout, align 8
  %2 = getelementptr inbounds i8, ptr %layout, i64 8
  store i64 %1, ptr %2, align 8
  %3 = getelementptr inbounds i8, ptr %layout, i64 8
  %_4 = load i64, ptr %3, align 8
  %4 = icmp eq i64 %_4, 0
  br i1 %4, label %bb2, label %bb1

bb2:                                              ; preds = %bb1, %start
  ret void

bb1:                                              ; preds = %start
  %5 = load i64, ptr %layout, align 8
  %6 = getelementptr inbounds i8, ptr %layout, i64 8
  %7 = load i64, ptr %6, align 8
  store i64 %5, ptr %layout1, align 8
  %8 = getelementptr inbounds i8, ptr %layout1, i64 8
  store i64 %7, ptr %8, align 8
  %_11 = load i64, ptr %layout, align 8
  %_14 = icmp uge i64 %_11, 1
  %_15 = icmp ule i64 %_11, -9223372036854775808
  %_16 = and i1 %_14, %_15
; call __rustc::__rust_dealloc
  call void @_RNvCsaNFudIaDR8x_7___rustc14___rust_dealloc(ptr %ptr, i64 %_4, i64 %_11) #15
  br label %bb2
}

; <alloc::boxed::Box<T,A> as core::ops::drop::Drop>::drop
; Function Attrs: inlinehint nonlazybind uwtable
define internal void @"_ZN72_$LT$alloc..boxed..Box$LT$T$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hba8de0dbeb02cdf5E"(ptr align 8 %self) unnamed_addr #1 {
start:
  %0 = alloca [8 x i8], align 8
  %1 = alloca [8 x i8], align 8
  %layout = alloca [16 x i8], align 8
  %ptr = load ptr, ptr %self, align 8
  store i64 4, ptr %1, align 8
  %size = load i64, ptr %1, align 8
  store i64 4, ptr %0, align 8
  %align = load i64, ptr %0, align 8
  br label %bb6

bb6:                                              ; preds = %start
; call core::alloc::layout::Layout::from_size_align_unchecked::precondition_check
  call void @_ZN4core5alloc6layout6Layout25from_size_align_unchecked18precondition_check17h8c791635264a3c10E(i64 %size, i64 %align, ptr align 8 @alloc_dce6ac8e6908cf2ed7e1e00c561d68e7) #15
  br label %bb7

bb7:                                              ; preds = %bb6
  %2 = getelementptr inbounds i8, ptr %layout, i64 8
  store i64 %size, ptr %2, align 8
  store i64 %align, ptr %layout, align 8
  %3 = icmp eq i64 %size, 0
  br i1 %3, label %bb3, label %bb1

bb3:                                              ; preds = %bb1, %bb7
  ret void

bb1:                                              ; preds = %bb7
  %_7 = getelementptr inbounds i8, ptr %self, i64 8
  %4 = load i64, ptr %layout, align 8
  %5 = getelementptr inbounds i8, ptr %layout, i64 8
  %6 = load i64, ptr %5, align 8
; call <alloc::alloc::Global as core::alloc::Allocator>::deallocate
  call void @"_ZN63_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Allocator$GT$10deallocate17h9ca9ff17ef3f71aaE"(ptr align 1 %_7, ptr %ptr, i64 %4, i64 %6)
  br label %bb3
}

; test_box::main
; Function Attrs: nonlazybind uwtable
define internal void @_ZN8test_box4main17h1ea34eea49858241E() unnamed_addr #0 personality ptr @rust_eh_personality {
start:
  %0 = alloca [16 x i8], align 8
; invoke alloc::alloc::exchange_malloc
  %_4.i = invoke ptr @_ZN5alloc5alloc15exchange_malloc17h74c37b16390a9bebE(i64 4, i64 4)
          to label %"_ZN5alloc5boxed12Box$LT$T$GT$3new17h72afce5fbc30bc12E.exit" unwind label %cleanup.i

cleanup.i:                                        ; preds = %start
  %1 = landingpad { ptr, i32 }
          cleanup
  %2 = extractvalue { ptr, i32 } %1, 0
  %3 = extractvalue { ptr, i32 } %1, 1
  store ptr %2, ptr %0, align 8
  %4 = getelementptr inbounds i8, ptr %0, i64 8
  store i32 %3, ptr %4, align 8
  %5 = load ptr, ptr %0, align 8
  %6 = getelementptr inbounds i8, ptr %0, i64 8
  %7 = load i32, ptr %6, align 8
  %8 = insertvalue { ptr, i32 } poison, ptr %5, 0
  %9 = insertvalue { ptr, i32 } %8, i32 %7, 1
  resume { ptr, i32 } %9

"_ZN5alloc5boxed12Box$LT$T$GT$3new17h72afce5fbc30bc12E.exit": ; preds = %start
  store i32 42, ptr %_4.i, align 4
; call core::mem::drop
  call void @_ZN4core3mem4drop17h9064afdcf4adc51bE(ptr align 4 %_4.i)
  ret void
}

; std::rt::lang_start_internal
; Function Attrs: nonlazybind uwtable
declare i64 @_ZN3std2rt19lang_start_internal17hca2222f6af67d268E(ptr align 1, ptr align 8, i64, ptr, i8) unnamed_addr #0

; Function Attrs: nounwind nonlazybind uwtable
declare i32 @rust_eh_personality(i32, i32, i64, ptr, ptr) unnamed_addr #4

; core::panicking::panic_in_cleanup
; Function Attrs: cold minsize noinline noreturn nounwind nonlazybind optsize uwtable
declare void @_ZN4core9panicking16panic_in_cleanup17h26d5998e8f171caaE() unnamed_addr #5

; core::panicking::panic_nounwind_fmt
; Function Attrs: cold noinline noreturn nounwind nonlazybind uwtable
declare void @_ZN4core9panicking18panic_nounwind_fmt17h991b22c5832142abE(ptr align 8, i1 zeroext, ptr align 8) unnamed_addr #6

; core::alloc::layout::Layout::is_size_align_valid
; Function Attrs: nonlazybind uwtable
declare zeroext i1 @_ZN4core5alloc6layout6Layout19is_size_align_valid17haacdf0c659f9060fE(i64, i64) unnamed_addr #0

; core::panicking::panic_cannot_unwind
; Function Attrs: cold minsize noinline noreturn nounwind nonlazybind optsize uwtable
declare void @_ZN4core9panicking19panic_cannot_unwind17h67b01b7815c95828E() unnamed_addr #5

; alloc::alloc::handle_alloc_error
; Function Attrs: cold minsize noreturn nonlazybind optsize uwtable
declare void @_ZN5alloc5alloc18handle_alloc_error17h503507c06789fd82E(i64, i64) unnamed_addr #7

; __rustc::__rust_no_alloc_shim_is_unstable_v2
; Function Attrs: nounwind nonlazybind uwtable
declare void @_RNvCsaNFudIaDR8x_7___rustc35___rust_no_alloc_shim_is_unstable_v2() unnamed_addr #4

; __rustc::__rust_alloc
; Function Attrs: nounwind nonlazybind allockind("alloc,uninitialized,aligned") allocsize(0) uwtable
declare noalias ptr @_RNvCsaNFudIaDR8x_7___rustc12___rust_alloc(i64, i64 allocalign) unnamed_addr #8

; __rustc::__rust_alloc_zeroed
; Function Attrs: nounwind nonlazybind allockind("alloc,zeroed,aligned") allocsize(0) uwtable
declare noalias ptr @_RNvCsaNFudIaDR8x_7___rustc19___rust_alloc_zeroed(i64, i64 allocalign) unnamed_addr #9

; __rustc::__rust_dealloc
; Function Attrs: nounwind nonlazybind allockind("free") uwtable
declare void @_RNvCsaNFudIaDR8x_7___rustc14___rust_dealloc(ptr allocptr, i64, i64) unnamed_addr #10

; Function Attrs: nonlazybind
define i32 @main(i32 %0, ptr %1) unnamed_addr #11 {
top:
  %2 = sext i32 %0 to i64
; call std::rt::lang_start
  %3 = call i64 @_ZN3std2rt10lang_start17hb1e4731a654e3d4cE(ptr @_ZN8test_box4main17h1ea34eea49858241E, i64 %2, ptr %1, i8 0)
  %4 = trunc i64 %3 to i32
  ret i32 %4
}

attributes #0 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #1 = { inlinehint nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #2 = { noinline nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #3 = { inlinehint nounwind nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #4 = { nounwind nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #5 = { cold minsize noinline noreturn nounwind nonlazybind optsize uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #6 = { cold noinline noreturn nounwind nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #7 = { cold minsize noreturn nonlazybind optsize uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #8 = { nounwind nonlazybind allockind("alloc,uninitialized,aligned") allocsize(0) uwtable "alloc-family"="__rust_alloc" "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #9 = { nounwind nonlazybind allockind("alloc,zeroed,aligned") allocsize(0) uwtable "alloc-family"="__rust_alloc" "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #10 = { nounwind nonlazybind allockind("free") uwtable "alloc-family"="__rust_alloc" "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #11 = { nonlazybind "target-cpu"="x86-64" }
attributes #12 = { cold }
attributes #13 = { cold noreturn nounwind }
attributes #14 = { noreturn nounwind }
attributes #15 = { nounwind }
attributes #16 = { noreturn }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 7, !"PIE Level", i32 2}
!2 = !{i32 2, !"RtLibUseGOT", i32 1}
!3 = !{!"rustc version 1.90.0-nightly (a2d45f73c 2025-07-07)"}
!4 = !{i64 8597244628339459}
