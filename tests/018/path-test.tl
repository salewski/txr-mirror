(load "../common")

(mtest
  (ends-with "/bin/sh" (path-search "sh")) t
  (path-search "AlMoStCeRtAiNlLyNoNeXisTenT") nil
  (path-search "") nil
  (path-search "sh" nil) nil
  (path-search "sh" '("AlMoStCeRtAiNlLyNoNeXisTenT")) nil
  (path-search "sh" '("AlMoStCeRtAiNlLyNoNeXisTenT" "/bin")) "/bin/sh"
  (path-search "sh" '("/bin")) "/bin/sh"
  (path-search "sh" "AlMoStCeRtAiNlLyNoNeXisTenT") nil
  (path-search "sh" "AlMoStCeRtAiNlLyNoNeXisTenT:/bin") "/bin/sh"
  (path-search "sh" "/bin") "/bin/sh"
  (path-search "sh" "/bin/") "/bin/sh"
  (path-search "sh" ":/bin/") "/bin/sh")
