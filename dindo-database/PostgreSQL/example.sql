--------------------------------------------------------------------------------
-- Dindo Database
-- PostgreSQL
-- PL/pgSQL
-- (C) DindoLab
-- sql/example.sql
--------------------------------------------------------------------------------


-- 插入样例用户
INSERT INTO table_account(key_uid,key_name,key_tel,key_pash)
  VALUES ( 'U201604054a8974a8974a8974a8974a8974a8974a8974a897'
         , 'qinka'
         , '15029056156'
         , '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
         )
;
INSERT INTO table_account(key_uid,key_name,key_tel,key_pash)
  VALUES ( 'U201604054a8954a8974a8974a8974a8974a8974a8974a897'
         , 'qinka2'
         , '10000000000'
         , '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
         )
;

-- 登陆人工添加临时 Token
INSERT INTO table_tmptoken(key_tmptoken,key_timeup,key_uid)
  VALUES ( '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
         , '2017-01-01 00:00:00-01'
         , 'U201604054a8974a8974a8974a8974a8974a8974a8974a897'
         )
;

INSERT INTO table_usr(key_uid,key_email,key_rname,key_prcid,key_addr,key_status)
  VALUES ( 'U201604054a8974a8974a8974a8974a8974a8974a8974a897'
         , 'qinka#live.com'
         , 'John Lee'
         , '140105199500000000'
         , 'shajdkhaskjdhaskdjh'
         , 'P'
         )
;
