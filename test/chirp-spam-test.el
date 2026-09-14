;;; chirp-spam-test.el --- Spam rule tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'chirp-spam)

(ert-deftest chirp-spam-user-rules-load-and-deduplicate ()
  "Persistent rules should ignore comments, blanks, and case duplicates."
  (let ((file (make-temp-file "chirp-spam-rules-")))
    (unwind-protect
        (let ((chirp-spam-rules-file file))
          (with-temp-file file
            (insert "# Local rules\n\n  Promo Name  \nspam_handle\nSPAM_HANDLE\n"))
          (should (equal (chirp-spam--read-user-rules)
                         '("Promo Name" "spam_handle"))))
      (delete-file file))))

(ert-deftest chirp-spam-rules-default-to-collected-templates ()
  "Spam defaults should contain accepted templates and omit unsafe terms."
  (dolist (template '("三网优化专线"
                      "刚放个人主页上了"
                      "比她好看的没她骚"
                      "我福不黑不信你看"
                      "应该没人比我玩的开"
                      "應該沒人比我玩得開"
                      "线下sao货"
                      "返佣"
                      "比我好看的没我骚"
                      "有人想锐评一下我的福嘛"
                      "check my bio asappp"
                      "dm me or follow back"
                      "no upfront payment is required until after a successful recovery"))
    (should (member template chirp-spam-rules)))
  (should (member '("体制内幼师" "sao的很")
                  chirp-spam-rules))
  (dolist (template '(("FoxLink" "银狐")
                      ("找炮友" "点主页")
                      ("同城上门" "线下选妃")
                      ("Gate" "Visa卡")
                      ("催情" "听话")
                      ("只入身体" "不入生活")))
    (should (member template chirp-spam-rules)))
  (dolist (broad-term '("主页" "私信" "微信" "带单" "稳赚" "空投" "DM me"))
    (should-not (member broad-term chirp-spam-rules)))
  (dolist (legitimate-template '("空投详情现已公布" "更多详情请查看公告"))
    (should-not (member legitimate-template chirp-spam-rules))))

(ert-deftest chirp-spam-default-rules-have-no-redundant-overlap ()
  "Default spam rules should not duplicate or subsume one another."
  (let ((rules chirp-spam-rules-default))
    (cl-labels
        ((normalized (fragment)
           (and (stringp fragment)
                (let ((trimmed (string-trim fragment)))
                  (unless (string-empty-p trimmed)
                    trimmed))))
         (fragments (rule)
           (if (listp rule) rule (list rule)))
         (fragment-subsumes-p (broad narrow)
           (when-let* ((broad (normalized broad))
                       (narrow (normalized narrow)))
             (let ((case-fold-search t))
               (string-match-p (regexp-quote broad) narrow))))
         (rule-subsumes-p (broad narrow)
           (let ((broad-fragments (fragments broad))
                 (narrow-fragments (fragments narrow)))
             (or (null narrow-fragments)
                 (and broad-fragments
                      (cl-every
                       (lambda (broad-fragment)
                         (cl-some
                          (lambda (narrow-fragment)
                            (fragment-subsumes-p
                             broad-fragment narrow-fragment))
                          narrow-fragments))
                       broad-fragments))))))
      (should-not
       (cl-loop for tail on rules
                thereis
                (cl-some
                 (lambda (other)
                   (or (rule-subsumes-p (car tail) other)
                       (rule-subsumes-p other (car tail))))
                 (cdr tail)))))))

(ert-deftest chirp-spam-default-rules-match-collected-variants ()
  "Default spam rules should match collected evasive variants."
  (dolist (text '("线下sao货没人pK比她sao😂👌 y7N"
                  "线下sao货pK比她sao🎍🎼比我骚的没我好看"
                  "比我好看的没我骚🎍🎼比我骚的没我好看"
                  "我果然太涩了🌜🤲有人想锐评一下我的福嘛"
                  "FoxLink🚀银狐全球高速连接"
                  "体制内幼师🌻📣sao的很Q1"))
    (should (chirp-spam-match-p (list :text text) chirp-spam-rules)))
  (dolist (text '("我是一名体制内幼师"
                  "空投详情现已公布"
                  "更多详情请查看公告"))
    (should-not (chirp-spam-match-p (list :text text) chirp-spam-rules))))

(ert-deftest chirp-spam-default-rules-match-collected-author-variants ()
  "Default spam rules should match collected author-name templates."
  (dolist (name '("深币Deepcoin93%大户返佣"
                  "FoxLink银狐全球高速连接"
                  "草莓熊🍑找炮友🍑点主页🍑"
                  "方露🌸同城上门♥线下选妃"
                  "返85 Gate·Visa卡可领"
                  "返佣85·Gate｜Visa卡免费"
                  "👈催情💊春💊男用💊听话 🧳 🎌 🍀"))
    (should
     (chirp-spam-match-p
      (list :text "普通回复" :author-name name) chirp-spam-rules)))
  (dolist (text '("只入身体🌱🪐不入生活。"
                  "只入身体🍁🍁不入生活。"))
    (should (chirp-spam-match-p (list :text text) chirp-spam-rules)))
  (dolist (name '("FoxLink 服务"
                  "银狐读书会"
                  "Deepcoin 使用体验"
                  "Gate 平台"
                  "Visa卡可领"
                  "同城生活"
                  "请勿轻信催情药"))
    (should-not
     (chirp-spam-match-p
      (list :text "普通回复" :author-name name) chirp-spam-rules))))

(ert-deftest chirp-spam-rule-groups-require-every-fragment ()
  "Grouped spam rules should require every configured fragment."
  (let ((chirp-spam-rules '(("体制内幼师" "sao的很"))))
    (should
     (chirp-spam-match-p
      '(:text "体制内幼师🌻📣sao的很Q1") chirp-spam-rules))
    (should-not
     (chirp-spam-match-p '(:text "体制内幼师的日常") chirp-spam-rules))
    (should-not
     (chirp-spam-match-p '(:text "这个说法 sao的很") chirp-spam-rules))))

(ert-deftest chirp-spam-rules-match-author-nickname-and-handle ()
  "Spam rules should inspect reply author display names and handles."
  (let ((chirp-spam-rules '("推广昵称" "spam_handle")))
    (should
     (chirp-spam-match-p
      '(:text "普通回复" :author-name "这是推广昵称" :author-handle "alice") chirp-spam-rules))
    (should
     (chirp-spam-match-p
      '(:text "普通回复" :author-name "Alice" :author-handle "Spam_Handle_01") chirp-spam-rules))
    (should-not
     (chirp-spam-match-p
      '(:text "普通回复" :author-name "Alice" :author-handle "alice") chirp-spam-rules))
    (should
     (chirp-spam-match-p
      '(:text "推广昵称" :author-name "推广昵称" :author-handle "spam_handle"
        :timeline-context related) chirp-spam-rules))))

(ert-deftest chirp-spam-normalizes-whitespace-without-crossing-fields ()
  "Captured multiline phrases match their source, but not adjacent fields."
  (should (equal (chirp-spam-normalize "  Hello\n  WORLD ") "Hello WORLD"))
  (should (chirp-spam-match-p '(:text "Hello\n  WORLD") '("hello world")))
  (should (chirp-spam-match-p '(:text "hello world") '("Hello\tWORLD")))
  (should-not (chirp-spam-match-p '(:text "hello" :author-name "world")
                                  '("hello world")))
  (should (chirp-spam-match-p '(:text "hello" :author-name "world")
                              '(("hello" "world"))))
  (should-not (chirp-spam-match-p '(:text "hello world") nil))
  (should-not (chirp-spam-match-p '(:text "hello world") '(("hello" " ")))))

(provide 'chirp-spam-test)
;;; chirp-spam-test.el ends here
