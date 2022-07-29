(load "../common")

;; only root can do this test
(unless (zerop (geteuid))
  (exit))

(defvarl testdir (mkdtemp `/tmp/txr-path-safe-test`))

(push-after-load (remove-path-rec testdir))

(chmod testdir "a+rX")

(defvarl atestdir (realpath testdir))
(defvarl tmpdir (path-cat testdir "tmp"))

(mkdir tmpdir)
(defvarl atmpdir (realpath tmpdir))
(ensure-dir tmpdir)
(chmod tmpdir "a+rwt")

(seteuid 10000)
(touch (path-cat tmpdir "10000"))
(symlink "/" (path-cat tmpdir "10000-link"))
(seteuid 0)

(seteuid 20000)
(touch (path-cat tmpdir "20000"))
(symlink "/" (path-cat tmpdir "20000-link"))
(seteuid 0)

(mtest
  (path-components-safe tmpdir) t
  (path-components-safe (path-cat tmpdir "10000")) nil
  (path-components-safe (path-cat tmpdir "10000-link")) nil
  (path-components-safe (path-cat tmpdir "20000")) nil)

(mtest
  (path-components-safe atmpdir) t
  (path-components-safe (path-cat atmpdir "10000")) nil
  (path-components-safe (path-cat atmpdir "10000-link")) nil
  (path-components-safe (path-cat atmpdir "20000")) nil)

(seteuid 10000)

(mtest
  (path-components-safe atmpdir) t
  (path-components-safe (path-cat tmpdir "10000")) t
  (path-components-safe (path-cat tmpdir "10000-link")) t
  (path-components-safe (path-cat tmpdir "20000")) nil
  (path-components-safe (path-cat tmpdir "20000-link")) nil)

(mtest
  (path-components-safe atmpdir) t
  (path-components-safe (path-cat atmpdir "10000")) t
  (path-components-safe (path-cat atmpdir "10000-link")) t
  (path-components-safe (path-cat atmpdir "20000")) nil
  (path-components-safe (path-cat atmpdir "20000-link")) nil)

(symlink "loop/x/y" (path-cat tmpdir "loop"))

(test
  (path-components-safe (path-cat tmpdir "loop/z")) :error)

(chdir tmpdir)
(symlink "b/c" "a")
(ensure-dir "b")
(symlink "x" "b/c")
(touch "b/x")

(test
  (path-components-safe "a") t)

(remove-path "b/c")

(test
  (path-components-safe "a") :error)

(seteuid 0)
(seteuid 20000)
(symlink "x" "z")

(seteuid 0)
(rename-path "z" "b/c")
(seteuid 10000)

(test
  (path-components-safe "a") nil)

(mtest
  (path-components-safe "/proc/1") t
  (path-components-safe "/proc/1/cwd") :error
  (path-components-safe "/proc/self/cwd") t)

(seteuid 0)

(mtest
  (path-components-safe "/proc/1") t
  (path-components-safe "/proc/1/fd") t
  (path-components-safe "/proc/sys/../1") t
  (path-components-safe "/proc/1/cwd") nil
  (path-components-safe "/proc/1/cwd/foo") nil
  (path-components-safe "/proc/self/cwd") nil
  (path-components-safe "/proc/self/cwd/foo") nil
  (path-components-safe "/proc/1/root") nil
  (path-components-safe "/proc/1/root/foo") nil
  (path-components-safe "/proc/1/fd/0") nil
  (path-components-safe "/proc/1/fd/0/bar") nil
  (path-components-safe "/proc/1/map_files") nil
  (path-components-safe "/proc/1/map_files/bar") nil
  (path-components-safe "/proc/sys/../1/cwd") nil
  (path-components-safe "/proc/1/task/1") t
  (path-components-safe "/proc/1/task/1/fd/0") nil
  (path-components-safe "/proc/1/task/1/cwd") nil
  (path-components-safe "/proc/1/task/1/root") nil)
