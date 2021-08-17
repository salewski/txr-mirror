;; Copyright 2020-2021
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
(defvarl sys:%quips%
  #("TXR was taped live before a studio audience. The laughter is genuine."
    "Exclusive of recycled stuffing, TXR contains new materials only."
    "TXR is not a toy, but should be kept within easy reach of children."
    "Do not remove this label until TXR is delivered to the consumer."
    "Remove TXR any time for a complete refund of your disk space."
    "Store TXR in a cool, dry storage device. Keep out of direct sunlight."
    "Do not operate heavy equipment or motor vehicles while using TXR."
    "Use full-strength TXR for tough tasks, or dilute with shell scripts."
    "TXR causes vomiting if accidentally ingested; no need to induce such."
    "If unwanted side effects persist, discontinue imperative programming."
    "TXR works even if the application surface is not free of dirt and grease."
    "TXR may be used in areas that are not necessarily well ventilated."
    "TXR's button pops up when original seal is broken."
    "TXR is tested on nothing but animals (so to speak)."
    "Disclaimer: TXR has been found to cure cancer in lab mice only."
    "Garbage collection is on Tuesdays: bring unwanted pointers to curb by 7:30."
    "Warning: may explode if heated, cooled or maintained at room temperature."
    "TXR is made with 75% post-consumer recycled cons cells."
    "Poke a few holes in TXR with a fork before heating in the microwave."
    "Caution: objects in heap are farther from reality than they appear."
    "TXR doesn't really whip the llama's ass so much as the lambda's."
    "TXR is recommended for either internal or external use. Whatever, y'know?"
    "TXR is enteric coated to release over 24 hours of lasting relief."
    "TXR contains many small parts, unsuitable for children under 12 months."
    "TXR is packaged by the byte; contents may compress during shipping."
    "Discontinue coding TXR if experiencing dizziness or shortness of breath."
    "Self-assembly keeps TXR costs low; but ask about our installation service!"
    "Some mild discoloration of syntax highlighting may occur with age."
    "TXR is made with equipment not contaminated by peanuts ... r-r-right?"
    "TXR is believed by fools to be free of defects in workmanship and materials."
    "Adults using TXR should be closely supervised by children."
    "TXR may be worn in seven different ways, in any weather."
    "TXR is light and portable; take it camping, or to the Bahamas."
    "Psst! The complimentary Allen key that comes with TXR is inpired by IKEA."
    "Ethically produced using volunteer geek labor in a first world country."
    "Program contains violence and coarse language, demanding user indiscretion."
    "TXR is written, directed and produced by, not to mention starring, Kaz."
    "Emergency exits are located in the standard library."
    "Your history may used for automatic recommendations, like 'stick to Python'."
    "Without the generosity of users like you, this program would exist just fine."
    "TXR's no-spray organic production means every bug is carefully removed by hand."
    "Upgrade to TXR Pro for a one-time fee of learning Lisp!"
    "When transferring between containers, do not siphon TXR by mouth."
    "Use TXR only as directed. Unless you are intelligent, curious and creative."
    "Reminder: your account balance of 37 closing parentheses is past due."
    "Check with your physician before commencing any strenuous Lisp program."
    "Apply today for a TXR credit card, and get 1MB off your next allocation."
    "Join TXR Rewards now, and get 15000 closing parentheses you can use anywhere."
    "TXR's car insurance now offers cdr coverage for bumper-to-bumper protection."
    "Please listen carefully to the following spec, as our Lisp has changed."
    "If TXR were TV, it would require music by Mike Post and Peter Carpenter."
    "TXR was set to appear on the cover of SI. Yeah, no. Learned about the jinx."
    "  |E|\n|V A N|\n  |S|\n  |T|"
    "Lispが好き、とても好き、私はLispの...全てにいつも...夢中なの。"
    "こんな広い分野の中、私が愛する...言語なら、やはり...Lispだけ。"
    "This area is under 24 hour TTY surveillance."
    "Imitation is the benignest form of forgery."
    "This could be the year of the TXR desktop; I can feel it!"
    "TXR's suggested retail price is $0. (More in Canada.)"
    "All of our cores are currently busy servicing other applications."
    "For faster service, do not time out & retry. Syscalls are answered in sequence."
    ))

(defparml sys:%quip-rand-state% (make-random-state))

(defvarl sys:%shuffled-quips%)

(defun quip ()
  (unless sys:%shuffled-quips%
    (set sys:%shuffled-quips% (shuffle sys:%quips% sys:%quip-rand-state%)))
  (pop sys:%shuffled-quips%))
