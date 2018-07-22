Feature:
  Simple typing
  Switch language state
  Switch character width

  Scenario: Simple typing
    Given I switch to a clean buffer "*mini-wubi-test*"
    Given I activate input method
    When I type "q j k l w"
    Then I should see "我是中国人"


  Scenario: Switch language state
    Given I switch to a clean buffer "*mini-wubi-test*"
    Given I activate input method
    When I call "mini-wubi-switch-lang-state"
    When I type "english words only."
    Then I should see "english words only."
    When I call "mini-wubi-switch-lang-state"
    When I type "q j k l w."
    Then I should see "我是中国人."


  Scenario: Switch character width
    Given I switch to a clean buffer "*mini-wubi-test*"
    Given I activate input method
    When I type "q j k l w."
    Then I should see "我是中国人."
    When I call "mini-wubi-switch-character-width"
    When I type "q j k l w."
    Then I should see "我是中国人。"
    When I type "q2b2"
    Then I should see "金子"