(load "../common")

(mtest
  (path-search "sh") "/bin/sh"
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
